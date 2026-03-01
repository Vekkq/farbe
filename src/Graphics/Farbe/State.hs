{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.State where

import Graphics.Farbe.Window
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.DMap
-- ~ import Graphics.Farbe.Shader

import qualified Data.Sequence as Seq
-- ~ import qualified Data.Map as M
import Graphics.GL.Types

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS
import GHC.Clock
import Numeric


import Data.Hashable
import qualified Data.IntMap.Strict as M
import Control.Monad.State.Strict
import System.Mem.StableName



newtype FarbeT m a = FarbeT { unFarbeT :: StateT FarbeState m a }
	deriving
		( Functor, Applicative, Monad, MonadIO
		, MonadReader r, MonadWriter w
		, MonadWindow
		)

instance MonadState s m => MonadState s (FarbeT m) where state = lift . state

instance MonadTrans FarbeT where lift = FarbeT . lift

class MonadIO m => Farbe m where
	stateFarbe :: (FarbeState -> (a, FarbeState)) -> m a

	getsFarbe :: (FarbeState -> a) -> m a
	getsFarbe f = stateFarbe $ \s -> (f s, s)

	getFarbe :: m FarbeState
	getFarbe = stateFarbe (\s -> (s,s))

	putFarbe :: FarbeState -> m ()
	putFarbe s = stateFarbe (\_ -> ((),s))

	modifyFarbe :: (FarbeState -> FarbeState) -> m ()
	modifyFarbe f = stateFarbe $ (\s -> ((), f s))

instance MonadIO m => Farbe (FarbeT m) where
	stateFarbe = FarbeT . state


count :: Farbe m => m Int
count = stateFarbe (\s -> let c = counter s in (c, s { counter = succ c }))


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\

SIMPLEFUNCTION_CLASSINSTANCES(stateFarbe,Farbe,.)

data FarbeState = FarbeState
	{ config :: Config
	, counter :: Int
	, vboState :: VBOState
	, texState :: TexState
	, delayed :: Seq.Seq (FarbeT IO ())
	, shaderCache :: DMap ShExec
	, lastFrameTime :: Double
	}

type ShaderId = GLuint

type ShExec = (ShaderId, FarbeT IO ())

stateShaderCache :: Farbe m
	=> (DMap ShExec -> (a, DMap ShExec)) -> m a
stateShaderCache f = stateFarbe $ \s ->
	let (a,c) = f $ shaderCache s in (a, s{ shaderCache = c })

getShaderCache :: Farbe m => m (DMap ShExec)
getShaderCache = stateShaderCache $ \s -> (s,s)

putShaderCache :: Farbe m => (DMap ShExec) -> m ()
putShaderCache d = stateShaderCache $ \_ -> ((),d)

modifyShaderCache :: (MonadIO m, Farbe m)
	=> (DMap ShExec -> m (DMap ShExec)) -> m ()
modifyShaderCache f = do
	sc <- getShaderCache
	sc' <- f sc
	putShaderCache sc'

data Config = Config
	{ debugMode :: Bool
	, printShaderPrograms :: Bool
	, workTime :: Double
	}

defaultConfig = Config
	{ debugMode = True
	, printShaderPrograms = True
	, workTime = 1/50
	}

emptyFarbeState :: MonadIO m => m FarbeState
emptyFarbeState = do
	vbo <- initHandVBOState (2^24)
	tex <- initTexState
	return $ FarbeState
		{ config = defaultConfig
		, counter = 0
		, vboState = vbo
		, texState = tex
		, delayed = Seq.empty
		, shaderCache = empty
		, lastFrameTime = 0
		}

runFarbeT :: FarbeState -> FarbeT m a -> m (a, FarbeState)
runFarbeT fs (FarbeT m) = runStateT m fs

getsConfig :: Farbe m => (Config -> s) -> m s
getsConfig f = getsFarbe (f . config)



printOn :: (Farbe m, MonadIO m) => (Config -> Bool) -> String -> m ()
printOn f s = do
	b <- getsConfig f
	when b $ liftIO $ putStrLn s

debug :: (Farbe m, MonadIO m, Show a) => a -> m ()
debug = printOn debugMode . show

devDebug :: (Farbe m, MonadIO m) => String -> m ()
devDebug = printOn printShaderPrograms

logTime :: (MonadWindow m, Farbe m, MonadIO m) => m ()
logTime = do
	t <- getTime
	modifyFarbe $ \s -> s { lastFrameTime = t }

-- ~ logItemIO :: Farbe m => m (String -> IO ())
-- ~ logItemIO = do
	-- ~ b <- getsConfig printShaderPrograms
	-- ~ t <- liftIO $ getMonotonicTime
	-- ~ return $ \s -> when b $ putStrLn $ "[" ++ showFFloat (Just 3) t [] ++ "] " ++ s


delay :: Farbe m => FarbeT IO () -> m ()
delay m = modifyFarbe $ \s -> s { delayed = delayed s Seq.|> m }


instance (MonadIO m, Farbe m) => HandVBO m where
	stateVBO f = stateFarbe (\s -> let (a,s') = f $ vboState s in (a, s{ vboState = s' } ))

instance (MonadIO m, Farbe m) => HandTex m where
	stateTex f = stateFarbe (\s -> let (a,s') = f $ texState s in (a, s{ texState = s' } ))

type Hash = Int

data DMap a = DMap
	{ stableNameMap :: M.IntMap a
	, backupMap :: M.IntMap (Hash, a)
	}

empty = DMap M.empty M.empty

-- this tuple holds the unchanged key for stablenaming
-- and a computation producing a hash from a for hashing
data DMapKey a m = DMapKey a (m Hash)

snHash :: MonadIO m => a -> m Hash
snHash a = liftIO $ hashStableName <$> makeStableName a


hashs :: (MonadIO m, Hashable a) => a -> m (Hash, Hash)
hashs a = do
	h1 <- snHash a
	let h2 = hash a
	return (h1,h2)


insertdm :: (MonadIO m, Hashable k) => k -> a -> DMap a -> m (DMap a)
insertdm k e (DMap m1 m2) = do
	h1 <- snHash k
	let h2 = hash k
	return $ DMap (M.insert h1 e m1) (M.insert h2 (h1,e) m2)


pickFirst :: [Maybe a] -> Maybe a
pickFirst (Just x:xs) = Just x
pickFirst (_:xs) = pickFirst xs
pickFirst _ = Nothing


lookupdm :: (Farbe m) => DMapKey k m -> m (Maybe ShExec)
lookupdm (DMapKey k1 k2) = do
	d@(DMap m1 m2) <- shaderCache <$> getFarbe
	h1 <- snHash k1
	case M.lookup h1 m1 of
		Just r -> return $ Just r
		Nothing -> do
			h2 <- hash <$> k2
			case M.lookup h2 m2 of
				Nothing -> return Nothing
				Just (h,a) -> do
					-- todo, add m2' to Farbe
					let m2' = M.insert h2 (h1,a) m2
					modifyFarbe $ \fd -> fd { shaderCache = DMap m1 m2' }
					return $ Just a


delete :: (MonadIO m, Hashable k) => k -> DMap a -> m (DMap a)
delete k (DMap m1 m2) = do
	(h1,h2) <- hashs k
	return $ DMap (M.delete h1 m1) (M.delete h2 m2)


