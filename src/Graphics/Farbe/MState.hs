{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.MState where

import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C


import Graphics.GL.Embedded20
import Graphics.GL.Types

import Graphics.Farbe.Window

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats



newtype MStateT s m a = MStateT { unMStateT :: ReaderT (MVar s) m a } deriving
	( Functor, Applicative, Monad, Alternative, MonadTrans
	, MonadWriter w, MonadError e, MonadIO
	, MonadWindow
	)


runMStateT :: MonadIO m => s -> MStateT s m a -> m a
runMStateT s m = do
	v <- liftIO $ newMVar s
	runReaderT (unMStateT m) v

instance (MonadReader r m, Monad m) => MonadReader r (MStateT s m) where
	ask = lift ask
	local f = MStateT . mapReaderT (local f) . unMStateT


instance MonadIO m => MonadState s (MStateT s m) where
	get = MStateT ask >>= liftIO . readMVar
	put a = MStateT ask >>= void . liftIO . flip swapMVar a

class MonadState s m => MState s m where
	getMVar :: m (MVar s)

instance MonadIO m => MState s (MStateT s m) where
  getMVar = MStateT ask

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\

SIMPLEFUNCTION_CLASSINSTANCES(getMVar,MState r,)






