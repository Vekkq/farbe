{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.State where

import Graphics.Farbe.Window
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
-- ~ import Graphics.Farbe.Shader

import Data.Map
import Data.Dynamic
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

import System.Mem.StableName
import Control.Exception
import Control.Concurrent.MVar

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS

import Control.Monad.IO.Class

newtype FarbeT m a = FarbeT { unFarbeT :: StateT FarbeState m a }
	deriving
		( Functor, Applicative, Monad, MonadIO
		, MonadReader r, MonadWriter w
		, MonadWindow
		)

instance MonadState s m => MonadState s (FarbeT m) where state = lift . state

instance MonadTrans FarbeT where lift = FarbeT . lift

class Farbe m where
	farbeState :: (FarbeState -> (a, FarbeState)) -> m a

	farbeGets :: (FarbeState -> a) -> m a
	farbeGets f = farbeState $ \s -> (f s, s)

	farbeGet :: m FarbeState
	farbeGet = farbeState (\s -> (s,s))

	farbePut :: FarbeState -> m ()
	farbePut s = farbeState (\_ -> ((),s))

instance Monad m => Farbe (FarbeT m) where
	farbeState = FarbeT . state


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(farbeState,Farbe,.)

data FarbeState = FarbeState
	{ config :: Config
	-- ~ , counter :: Int
	, vboState :: VBOState
	, texState :: TexState
	, delayed :: Seq.Seq (FarbeT IO ())
	, shaderCache :: CacheState (FarbeT IO)
	, lastFrameTime :: Double
	}

data CacheState w = CacheState
	{ cacheMap :: M.Map Hash (MVar (w ())) -- holds items based on StableName
	, backupMap :: M.Map Hash (MVar (w ())) -- holds items based on partial hashes
	}

data Config = Config
	{ debugMode :: Bool
	, devDebugMode :: Bool
	, workTime :: Double
	}

emptyFarbeState = FarbeState
	{ config = Config True True (1/80)
	-- ~ , counter = 0
	, vboState = undefined
	, texState = undefined
	, delayed = undefined
	, shaderCache = undefined
	, lastFrameTime = undefined
	}

runFarbeT :: Functor m => FarbeT m a -> m a
runFarbeT (FarbeT m) = fst <$> runStateT m emptyFarbeState

runFarbeT' :: FarbeState -> FarbeT m a -> m (a, FarbeState)
runFarbeT' fs (FarbeT m) = runStateT m fs


type Hash = Int


instance (MonadIO m, Farbe m) => HandVBO m where
	stateVBO f = farbeState (\s -> let (a,s') = f $ vboState s in (a, s{ vboState = s' } ))

instance (MonadIO m, Farbe m) => HandTex m where
	stateTex f = farbeState (\s -> let (a,s') = f $ texState s in (a, s{ texState = s' } ))

