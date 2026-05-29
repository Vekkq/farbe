{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-type-defaults #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.State where

import Graphics.Farbe.Window
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Utility
import Graphics.GL.Types

import Control.Concurrent

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS
import GHC.Stack

import qualified Data.IntMap.Strict as M

import Graphics.GL.Embedded20



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

instance (MonadIO m, Farbe m) => Counter m where

-- ~ count :: Farbe m => m Int
	count = stateFarbe (\s -> let c = counter s in (c, s { counter = succ c }))


instance (MonadIO m, Farbe m) => HandVBO m where
	stateVBO f = stateFarbe (\s -> let (a,s') = f $ vboState s in (a, s{ vboState = s' } ))

instance (MonadIO m, Farbe m) => HandTex m where
	stateTex f = stateFarbe (\s -> let (a,s') = f $ texState s in (a, s{ texState = s' } ))

	getDelayFun :: MonadIO m => m (IO () -> IO ())
	getDelayFun = delayFun


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
	, delayed :: MVar (FarbeT IO ())
	, shaderCache :: M.IntMap ShExec
	, lastFrameTime :: Double
	}

data Config = Config
	{ debugMode :: Bool
	, devDebugMode :: Bool
	, workTime :: Double
	}

defaultConfig :: Config
defaultConfig = Config
	{ debugMode = False
	, devDebugMode = False
	, workTime = 1/50
	}

emptyFarbeState :: MonadIO m => m FarbeState
emptyFarbeState = do
	vbo <- initHandVBOState (2^24)
	tex <- initTexState
	del <- liftIO $ newEmptyMVar
	return $ FarbeState
		{ config = defaultConfig
		, counter = 0
		, vboState = vbo
		, texState = tex
		, delayed = del
		, shaderCache = M.empty
		, lastFrameTime = 0
		}

type ShaderId = GLuint

type ShExec = FarbeT IO Bool

runFarbeT :: FarbeState -> FarbeT m a -> m (a, FarbeState)
runFarbeT fs (FarbeT m) = runStateT m fs

-- | variant running with empty state
runFarbeT' :: MonadIO m => FarbeT m a -> m (a, FarbeState)
runFarbeT' m = do
	fs <- emptyFarbeState
	runFarbeT fs m

getsConfig :: Farbe m => (Config -> s) -> m s
getsConfig f = getsFarbe (f . config)

modifyConfig :: Farbe m => (Config -> Config) -> m ()
modifyConfig f = modifyFarbe (\farb -> farb { config = f $ config farb })


stateShaderCache :: Farbe m
	=> (M.IntMap ShExec -> (a, M.IntMap ShExec)) -> m a
stateShaderCache f = stateFarbe $ \s ->
	let (a,c) = f $ shaderCache s in (a, s{ shaderCache = c })

getShaderCache :: Farbe m => m (M.IntMap ShExec)
getShaderCache = stateShaderCache $ \s -> (s,s)

putShaderCache :: Farbe m => (M.IntMap ShExec) -> m ()
putShaderCache d = stateShaderCache $ \_ -> ((),d)

modifyShaderCache :: (Farbe m)
	=> (M.IntMap ShExec -> M.IntMap ShExec) -> m ()
modifyShaderCache f = do
	sc <- getShaderCache
	let sc' = f sc
	putShaderCache sc'


printOn :: (Farbe m, MonadIO m) => (Config -> Bool) -> String -> m ()
printOn f s = do
	b <- getsConfig f
	when b $ liftIO $ putStrLn s

debug :: (Farbe m, MonadIO m) => String -> m ()
debug = printOn debugMode

devDebug :: (Farbe m, MonadIO m) => String -> m ()
devDebug = printOn devDebugMode

logTime :: (MonadWindow m, Farbe m, MonadIO m) => m ()
logTime = do
	t <- getTime
	modifyFarbe $ \s -> s { lastFrameTime = t }


delay :: Farbe m => FarbeT IO () -> m ()
delay m = do --modifyFarbe $ \s -> s { delayed = delayed s Seq.|> m }
		d <- getsFarbe delayed
		liftIO $ putMVar d m

delayFun :: Farbe m => m (IO () -> IO ())
delayFun = do
	d <- getsFarbe delayed
	return $ putMVar d . lift



getThisLine :: HasCallStack => Int
getThisLine = case reverse $ getCallStack callStack of
	(_,cs):_ -> srcLocStartLine cs
	_ -> 0

glErr :: MonadIO m => m ()
glErr = liftIO $ glGetError >>= \e -> putStrLn $ "gl error: " ++ show e

