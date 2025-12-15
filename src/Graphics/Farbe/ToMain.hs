{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe.ToMain where


import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.Chan



-- ~ import Graphics.Farbe.GL
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)



newtype ToMainT a = ToMainT { unToMain :: ReaderT (Chan (IO ())) IO a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadIO
		)

class ToMain m where
	toMain :: IO () -> m ()

instance ToMain ToMainT where
	toMain io = do
		chan <- ToMainT ask
		liftIO $ writeChan chan io

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(toMain,ToMain,.)

runToMain :: ToMainT () -> IO ()
runToMain m = do
	chan <- newChan
	forkIO $ void $ runReaderT (unToMain m) chan
	void $ getChanContents chan >>= sequence










