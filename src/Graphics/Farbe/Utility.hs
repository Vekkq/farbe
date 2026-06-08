{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Utility where

import Graphics.Farbe.GL
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Foreign hiding (void)
import Foreign.C
import Control.Exception

import Control.Monad.IO.Class ()


class Counter m where
	count :: m Int

name :: (Counter m, Functor m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

nameAttrib :: (Counter m, Functor m, GLtype a) => String -> a -> m String
nameAttrib s a = (++ glShortName a) <$> generateName s

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f


generateName :: (Counter m, Functor m) => String -> m String
generateName s = (s++) . ("_"++) . show <$> count


catchMVarBlocked :: Int -> IO a -> IO a
catchMVarBlocked i = handle (\BlockedIndefinitelyOnMVar -> error (show i))


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

makeRunOnce' :: MonadIO m => m a -> m (m a)
makeRunOnce' m = do
	ro <- makeRunOnce m
	return $ runOnce ro


-- RunWhenChanged ------------------------------------------------------------------------

data RunWhenChanged m a = RunWhenChanged (a -> m ()) (MVar a)

makeRunWhenChanged :: MonadIO m => (a -> m2 ()) -> m (RunWhenChanged m2 a)
makeRunWhenChanged m = liftIO $ RunWhenChanged m <$> newEmptyMVar

runwc :: (MonadIO m, Eq a) => RunWhenChanged m a -> a -> m ()
runwc (RunWhenChanged f ml) a = do
	l <- liftIO $ tryReadMVar ml
	when (Just a /= l) $ do
		fuzzySwapMVar ml a
		f a

updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ fuzzySwapMVar m a

fuzzySwapMVar :: MonadIO m => MVar a -> a -> m (Maybe a)
fuzzySwapMVar ml a = liftIO $ do
	r <- tryTakeMVar ml
	putMVar ml a
	return r



withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m (a, b)
withPtr f = liftIO $ alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr_ f = fst <$> withPtr f

