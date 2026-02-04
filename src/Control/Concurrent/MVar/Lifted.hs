{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

module Control.Concurrent.MVar.Lifted where

{- |
	This module provides lifted versions of MVar functions. @modifyMVar@ is reimplemented without its exeception-safety.

-}

import Control.Monad.IO.Class
import qualified Control.Concurrent.MVar as M
import Data.Composition
import Control.Exception.Base



type MVar = M.MVar

newEmptyMVar :: MonadIO m => m (MVar a)
newMVar :: MonadIO m => a -> m (MVar a)
takeMVar :: MonadIO m => MVar a -> m a
putMVar :: MonadIO m => MVar a -> a -> m ()
readMVar :: MonadIO m => MVar a -> m a
swapMVar :: MonadIO m => MVar a -> a -> m a
tryTakeMVar :: MonadIO m => MVar a -> m (Maybe a)
tryPutMVar :: MonadIO m => MVar a -> a -> m Bool
isEmptyMVar :: MonadIO m => MVar a -> m Bool
modifyMVarIO_ :: MonadIO m => MVar a -> (a -> IO a) -> m ()
modifyMVarIO :: MonadIO m => MVar a -> (a -> IO (a, b)) -> m b
modifyMVar_ :: MonadIO m => MVar a -> (a -> m a) -> m ()
modifyMVar :: MonadIO m => MVar a -> (a -> m (a, b)) -> m b

#define makeFn(fn,par) fn = liftIO par M.fn

makeFn(newEmptyMVar, $)
makeFn(newMVar, .)
makeFn(takeMVar, .)
makeFn(putMVar, .:)
makeFn(readMVar, .)
makeFn(swapMVar, .:)
makeFn(tryTakeMVar, .)
makeFn(tryPutMVar, .:)
makeFn(isEmptyMVar, .)

modifyMVarIO_ = liftIO .: M.modifyMVar_
modifyMVarIO = liftIO .: M.modifyMVar

modifyMVar m f = do
	x <- takeMVar m
	(a,b) <- f x
	putMVar m a
	return b

modifyMVar_ m f = do
	x <- takeMVar m
	a <- f x
	putMVar m a
