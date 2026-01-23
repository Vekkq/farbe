{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

-- | This module is the original Control.Concurrent.Chan from @base@ with addition of @tryReadChan@ .
{-
The full text of the license of @base@ is reproduced below.
-----------------------------------------------------------------------------

The Glasgow Haskell Compiler License

Copyright 2004, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

-----------------------------------------------------------------------------

Code derived from the document "Report on the Programming Language
Haskell 98", is distributed under the following license:

  Copyright (c) 2002 Simon Peyton Jones

  The authors intend this Report to belong to the entire Haskell
  community, and so we grant permission to copy and distribute it for
  any purpose, provided that it is reproduced in its entirety,
  including this Notice.  Modified versions of this Report may also be
  copied and distributed for any purpose, provided that the modified
  version is clearly presented as such, and that it does not claim to
  be a definition of the Haskell 98 Language.

-----------------------------------------------------------------------------

Code derived from the document "The Haskell 98 Foreign Function
Interface, An Addendum to the Haskell 98 Report" is distributed under
the following license:

  Copyright (c) 2002 Manuel M. T. Chakravarty

  The authors intend this Report to belong to the entire Haskell
  community, and so we grant permission to copy and distribute it for
  any purpose, provided that it is reproduced in its entirety,
  including this Notice.  Modified versions of this Report may also be
  copied and distributed for any purpose, provided that the modified
  version is clearly presented as such, and that it does not claim to
  be a definition of the Haskell 98 Foreign Function Interface.

-----------------------------------------------------------------------------
-}

module Graphics.Farbe.Chan
  (
          -- * The 'Chan' type
        Chan,

          -- * Operations
        newChan,
        writeChan,
        readChan,
        tryReadChan,
        dupChan,

          -- * Stream interface
        getChanContents,
        writeList2Chan,
   ) where

import Prelude
import System.IO.Unsafe         ( unsafeInterleaveIO )
import Control.Concurrent.MVar
import Control.Exception (mask, mask_, evaluate, onException)
import Control.Monad (join)

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
  -- benchmarks show that unboxing the MVar here is worthwhile, because
  -- although it leads to higher allocation, the channel data takes up
  -- less space and is therefore quicker to GC.

-- See the Concurrent Haskell paper for a diagram explaining
-- how the different channel operations proceed.

-- @newChan@ sets up the read and write end of a channel by initialising
-- these two @MVar@s with an empty @MVar@.

-- |Build and return a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
   hole  <- newEmptyMVar
   readVar  <- newMVar hole
   writeVar <- newMVar hole
   return (Chan readVar writeVar)

-- To put an element on a channel, a new hole at the write end is created.
-- What was previously the empty @MVar@ at the back of the channel is then
-- filled in with a new stream element holding the entered value and the
-- new hole.

-- |Write a value to a 'Chan'.
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

-- |Read the next value from the 'Chan'. Blocks when the channel is empty. Since
-- the read end of a channel is an 'MVar', this operation inherits fairness
-- guarantees of 'MVar's (e.g. threads blocked in this operation are woken up in
-- FIFO order).
--
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


-- |Duplicate a 'Chan': the duplicate channel begins empty, but data written to
-- either channel from then on will be available from both. Hence this creates
-- a kind of broadcast channel, where data written by anyone is seen by
-- everyone else.
--
-- (Note that a duplicated channel is not equal to its original.
-- So: @fmap (c /=) $ dupChan c@ returns @True@ for all @c@.)
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
   hole       <- readMVar writeVar
   newReadVar <- newMVar hole
   return (Chan newReadVar writeVar)

-- Operators for interfacing with functional streams.

-- |Return a lazy list representing the contents of the supplied
-- 'Chan', much like 'GHC.Internal.System.IO.hGetContents'.
getChanContents :: Chan a -> IO [a]
getChanContents ch
  = unsafeInterleaveIO (do
        x  <- readChan ch
        xs <- getChanContents ch
        return (x:xs)
    )

-- |Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)
