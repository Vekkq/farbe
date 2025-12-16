{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Utils where

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

import Foreign hiding (void)
import Foreign.C



withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m (a, b)
withPtr f = liftIO $ alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr_ f = fst <$> withPtr f




