{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.State where

import Graphics.Farbe.Window

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
	, counter :: Int
	, vboState :: VBOState
	, texState :: TexState
	, delayed :: Seq.Seq (FarbeT IO ())
	, shaderCache :: CacheState (FarbeT IO)
	, lastFrameTime :: Double
	}



data Config = Config
	{ debugMode :: Bool
	, devDebugMode :: Bool
	, workTime :: Double
	}


data Pager n = Pager
	{ imap :: M.Map n n -- | position - length
	, lastCheck :: n
	} deriving (Read, Show, Eq, Ord)

data VBOState = VBOState
	{ pager :: Pager GLintptr
	, vboIndex :: GLuint
	}

data TexState = TexState
	{ lastUsed :: Word32
	, texArr :: (IOUArray Word32 GLuint)
	}

type Hash = Int

data CacheState w = CacheState
	{ cacheMap :: M.Map Hash (MVar (w ())) -- holds items based on StableName
	, backupMap :: M.Map Hash (MVar (w ())) -- holds items based on partial hashes
	}
