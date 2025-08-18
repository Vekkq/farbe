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

-- Pager - calculations for Buffer allocation --------------------------------------------

data Pager n = Pager
	{ imap :: M.Map n n -- | position - length
	, lastCheck :: n
	} deriving (Read, Show, Eq, Ord)


newPager :: Integral n
	=> n -- | total size
	-> Pager n
newPager s = Pager (M.fromList [((-1), 1), (s,negate s)]) 0

calcAlloc :: Integral n
	=> n -- | alignment
	-> Pager n
	-> n -- | size to be allocated
	-> Maybe (Pager n, n)
calcAlloc a mm@(Pager imap c) size
	| size > 0 = (\i -> (Pager (M.insert i size imap) (i+size), i)) <$> nextSpace a mm start size
	| otherwise = Nothing
	where
		start = fromMaybe (error "Pager: corrupted") $ uncurry (+) <$> M.lookupLE c imap

align :: (Integral a) => a -> a -> a
align a p
	| a <= 0 = p
	| mod p a == 0 = p
	| otherwise = p + a - mod p a

nextSpace :: Integral n => n -> Pager n -> n -> n -> Maybe n
nextSpace a (Pager imap c) start size =
	case (M.lookupLE c imap, M.lookupGE c imap) of
		(Just (p,l), Just (p2,l2))
			| aln <- (align a $ p+l), aln + size <= p2 -> Just aln
			| p+l == start -> Nothing
			| otherwise -> nextSpace a (Pager imap (p2+l2)) start size
		_ -> error "Pager: out of bounds"

calcLength :: Integral n => n -> Pager n -> n
calcLength k mm = fromMaybe 0 $ M.lookup k $ imap mm

calcRemove :: Integral n => n -> Pager n -> Pager n
calcRemove k (Pager imap c) = Pager imap' c
	where
		imap' = if k /= min' && k /= max' then M.delete k imap else imap
		min' = fst $ fromJust $ M.lookupMin imap
		max' = fst $ fromJust $ M.lookupMax imap

pagerSize :: Integral n => Pager n -> n
pagerSize = fst . fromJust . M.lookupMax . imap

------------------------------------------------------------------------------------------


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









