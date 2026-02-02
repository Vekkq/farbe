{-# LANGUAGE CPP #-}

module Graphics.Farbe.QChan
  (
          -- * The 'Chan' type
        Chan,

          -- * Operations
        newChan,
        writeChan,
        readChan,
        tryReadChan
   ) where

import Control.Concurrent.MVar
-- ~ import Control.Exception (mask, mask_, evaluate, onException)
-- ~ import Control.Monad (join)

#define _UPK_(x) {-# UNPACK #-} !(x)

-- A channel is represented by two @MVar@s keeping track of the two ends
-- of the channel contents, i.e., the read- and write ends. Empty @MVar@s
-- are used to handle consumers trying to read from an empty channel.

-- |'Chan' is an abstract type representing an unbounded FIFO channel.
data Chan a
 = Chan _UPK_(MVar (Stream a))
        _UPK_(MVar (Stream a)) -- Invariant: the Stream a is always an empty MVar
   deriving Eq -- ^ @since 4.4.0.0

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a _UPK_(Stream a)
-- |Build and return a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   readVar  <- newMVar hole
   writeVar <- newMVar hole
   return (Chan readVar writeVar)

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  new_hole <- newEmptyMVar
  mask_ $ do
    old_hole <- takeMVar writeVar
    putMVar old_hole (ChItem val new_hole)
    putMVar writeVar new_hole

-- The reason we don't simply do this:
--
--    modifyMVar_ writeVar $ \old_hole -> do
--      putMVar old_hole (ChItem val new_hole)
--      return new_hole
--
-- is because if an asynchronous exception is received after the 'putMVar'
-- completes and before modifyMVar_ installs the new value, it will set the
-- Chan's write end to a filled hole.

-- |Read the next value from the 'Chan'. Blocks when the channel is empty. 
-- Throws 'Control.Exception.BlockedIndefinitelyOnMVar' when the channel is
-- empty and no other thread holds a reference to the channel.
readChan :: Chan a -> IO a
readChan (Chan readVar _) =
  modifyMVar readVar $ \read_end -> do
    (ChItem val new_read_end) <- readMVar read_end
        -- Use readMVar here, not takeMVar,
        -- else dupChan doesn't work
    return (new_read_end, val)

isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan readVar _) = readMVar readVar >>= isEmptyMVar

tryReadChan :: Chan a -> IO (Maybe a)
tryReadChan (Chan readVar _) =
  fmap join $ tryModifyMVar readVar $ \read_end -> do
    maybeChItem <- tryReadMVar read_end
    return $ case maybeChItem of
      Just (ChItem val new_read_end) -> (new_read_end, Just val)
      Nothing -> (read_end, Nothing)
  

tryModifyMVar :: MVar a -> (a -> IO (a,b)) -> IO (Maybe b)
tryModifyMVar m io = mask $ \restore -> do
    ma <- tryTakeMVar m
    case ma of 
      Just a -> do
        (a',b) <- restore (io a >>= evaluate) `onException` putMVar m a
        putMVar m a'
        return $ Just b
      Nothing -> return $ Nothing

