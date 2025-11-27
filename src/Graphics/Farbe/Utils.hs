{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.Farbe.Utils where

import qualified Data.Map as M
import Data.Maybe
import Foreign hiding (void)
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader

-- ~ import Data.Typeable




-- RunOnce -------------------------------------------------------------------------------

data RunOnce m a = RunOnce (m a) (MVar a)

makeRunOnce :: MonadIO m => m2 a -> m (RunOnce m2 a)
makeRunOnce ma = liftIO $ RunOnce ma <$> newEmptyMVar

runOnce :: MonadIO m => RunOnce m a -> m a
runOnce (RunOnce m ma) = do
	maybea <- liftIO $ tryReadMVar ma
	case maybea of
		Just a -> return a
		Nothing -> do
			b <- m
			liftIO $ tryPutMVar ma b
			return b


-- RunWhenChanged ------------------------------------------------------------------------

data RunWhenChanged m a = RunWhenChanged (a -> m ()) (MVar a)

makeRunWhenChanged :: MonadIO m => (a -> m2 ()) -> m (RunWhenChanged m2 a)
makeRunWhenChanged m = liftIO $ RunWhenChanged m <$> newEmptyMVar

runwc :: (MonadIO m, Eq a) => RunWhenChanged m a -> a -> m ()
runwc (RunWhenChanged f ml) a = do
	l <- liftIO $ tryReadMVar ml
	if maybe False (a==) l
		then return ()
		else do
			fuzzySwapMVar ml a
			f a


updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ fuzzySwapMVar m a

fuzzySwapMVar :: MonadIO m => MVar a -> a -> m (Maybe a)
fuzzySwapMVar ml a = liftIO $ do
	r <- tryTakeMVar ml
	tryPutMVar ml a
	return r


withPtr :: Storable a => (Ptr a -> IO b) -> IO (a, b)
withPtr f = do
	alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: Storable a => (Ptr a -> IO ()) -> IO a
withPtr_ f = fst <$> withPtr f









