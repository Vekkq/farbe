{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe.GLScheduler where

import Graphics.Farbe.DChan
import Control.Monad.Reader
import Control.Concurrent



data ScheduleState m = ScheduleState
	{ fps :: Float
	, immediateChan :: DChan (m ())
	, delayedChan :: DChan (m ())
	}

newtype GLScheduler m a = GLScheduler
	{ runGLScheduler :: ReaderT (ScheduleState m) IO a }
	deriving
		(Functor, Applicative, Monad, MonadIO)


class Schedule t m | t -> m where
	immediate :: m () -> t ()
	delayed :: m () -> t ()

instance Schedule (GLScheduler m) m where
	immediate a = do
		c <- asks immediateChan
		writeDChan c a
	delayed a = do
		c <- asks delayedChan
		writeDChan c a

runScheduler :: MonadIO m => GLScheduler m a -> m ()
runScheduler m = do
	f <- newDChan
	d <- newDChan  
	let ss = ScheduleState 80 f d
	liftIO $ forkIO $ runReaderT (runGLScheduler m) ss 
	loop
	where
		loop t = do
			t' <- getTime
			display
			loop t'

			
whenTimeLeft t m = undefined
		



