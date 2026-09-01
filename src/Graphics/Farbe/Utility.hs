{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Utility where

import Control.Monad.Reader
import Foreign hiding (void)
import Foreign.C
import Control.Exception

import Control.Monad.IO.Class ()



withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f


catchMVarBlocked :: Int -> IO a -> IO a
catchMVarBlocked i = handle (\BlockedIndefinitelyOnMVar -> error (show i))



withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m (a, b)
withPtr f = liftIO $ alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr_ f = fst <$> withPtr f

