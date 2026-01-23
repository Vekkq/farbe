
module Graphics.Farbe.GLScheduler where

import Control.Concurrent.MVar



data GLScheduler = GLScheduler Int ScheduleState

data QChan = Chan (Either () (IO ())

data ScheduleState = ScheduleState 
	{ immediate :: QChan
	, delayed :: QChan
	}

newtype GLSchedulerT m a = GLSchedulerT
	{ runGLSchedulerT :: ReaderT (m ()) ScheduleState a }


class Schedule t m | t -> m where
	immediateIO :: m () -> t ()	delayedIO :: m () -> t ()

instance Schedule (GLSchedulerT m) m where
	immediateIO a = do 
		(ScheduleState c _) <- ask
		writeChan c $ right a
	delayedIO a = do
		(ScheduleState _ c) <- ask
		writeChan c $ right a

runScheduler :: MonadIO m => GLSchedulerT m -> m ()
runScheduler m = do
	f <- newEmptyMVar
	d <- newEmptyMVar  
	let ss = ScheduleState f d
	forkIO $ runReaderT (runGLSchedulerT m) ss $ 
	let loop t = do
		



