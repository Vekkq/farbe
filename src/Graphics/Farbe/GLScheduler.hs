{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe.GLScheduler where

import Graphics.Farbe.DChan
import Graphics.Farbe.Window
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Function


data ScheduleState m = ScheduleState
	{ immediateChan :: DChan (m ())
	, delayedChan :: DChan (m ())
	, fps :: Double
	, mvarLastFrameTime :: MVar Double
	}

newScheduleState :: (MonadWindow m, MonadIO m) => Double -> m (ScheduleState n)
newScheduleState f = do
	i <- newDChan
	d <- newDChan
	t <- getTime >>= liftIO . newMVar
	return $ ScheduleState i d f t

newtype GLScheduler m a = GLScheduler
	{ runGLScheduler :: ReaderT (ScheduleState m) IO a }
	deriving
		(Functor, Applicative, Monad, MonadIO)


class Schedule t m | t -> m where
	getSchedule :: t (ScheduleState m)

instance Schedule (GLScheduler m) m where
	getSchedule = GLScheduler $ ask

immediate :: (Schedule t m, MonadIO t) => m () -> t ()
immediate a = do
	c <- immediateChan <$> getSchedule
	writeDChan c a

delayed :: (Schedule t m, MonadIO t) => m () -> t ()
delayed a = do
	c <- delayedChan <$> getSchedule
	writeDChan c a


runScheduler :: (MonadWindow m, Schedule m m, MonadIO m)
	=> Double -> GLScheduler m a -> m a
runScheduler slot m = do
	ss <- newScheduleState slot
	mvar <- liftIO $ newEmptyMVar
	liftIO $ forkIO $ runReaderT (runGLScheduler m) ss >>= putMVar mvar
	fix $ \loop -> do
		runSchedule
		r <- liftIO $ tryTakeMVar mvar
		case r of
			Just a -> return a
			Nothing -> loop

runSchedule :: (MonadWindow m, Schedule m m, MonadIO m) => m ()
runSchedule = fix $ \loop -> do
	(ScheduleState _ _ slot mlast) <- getSchedule
	runScheduleTask
	last <- liftIO $ readMVar mlast
	t <- getTime
	if last + slot < t then loop else do
		swapBuffers
		t' <- getTime
		liftIO $ void $ swapMVar mlast t'


runScheduleTask :: (MonadIO m, Schedule m m) => m ()
runScheduleTask = do
	(ScheduleState i d _ _) <- getSchedule
	readAvailableDChan i >>= sequence_
	mt <- tryReadDChan d
	case mt of
		Just t -> t
		Nothing -> liftIO $ threadDelay 100 -- 0.0001s




-- ~ instance GLScheduler where

