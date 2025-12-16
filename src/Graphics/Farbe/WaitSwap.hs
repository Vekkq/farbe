{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.VertexArray where



import Foreign hiding (void)
import Foreign.C

import System.Mem.Weak

-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import Control.Concurrent



import Debug.Trace
import System.Mem




newtype WaitSwapT m a = WaitSwapT { unWaitSwap :: ReaderT (MVar ()) m a } deriving
  ( Functor, Applicative, Monad, MonadTrans, MonadIO )

runWaitSwap :: MonadIO m => WaitSwapT m a -> m a
runWaitSwap m = do
  v <- liftIO $ newEmptyMVar
  runReaderT (unWaitSwap m) v

class MonadIO m => WaitSwap m where
  waitSwap :: m a -> m a
  signalSwap :: m ()

instance MonadIO m => WaitSwap (WaitSwapT m) where
  waitSwap m = do
    v <- WaitSwapT ask
    liftIO $ readMVar v
    m
  signalSwap = do
    v <- WaitSwapT ask
    liftIO $ do
      putMVar v ()
      yield
      takeMVar v


#define SIMPLEFUNCTION2_CLASSINSTANCES(fn,op,fn2,op2,cn) \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn; fn2 = lift op2 fn2 }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn; fn2 = lift op2 fn2 } ;\

SIMPLEFUNCTION2_CLASSINSTANCES(waitSwap,.,signalSwap,,WaitSwap)





