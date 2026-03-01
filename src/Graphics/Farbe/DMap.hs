{-# OPTIONS_GHC -fno-warn-tabs #-}

module Graphics.Farbe.DMap where

import Data.Hashable
import qualified Data.IntMap.Strict as M
import Control.Monad.State.Strict
import System.Mem.StableName

import Debug.Trace




type Hash = Int

data DMap a = DMap
	{ stableNameMap :: M.IntMap a
	, hashMap :: M.IntMap a -- (Hash, a)
	}

empty = DMap M.empty M.empty


snHash :: MonadIO m => a -> m Hash
snHash a = liftIO $ hashStableName <$> makeStableName a


hashs :: (MonadIO m, Hashable a) => a -> m (Hash, Hash)
hashs a = do
	h1 <- snHash a
	let h2 = hash a
	return (h1,h2)


insert :: (MonadIO m, Hashable k) => k -> a -> DMap a -> m (DMap a)
insert k e (DMap m1 m2) = do
	(h1,h2) <- hashs k
	return $ DMap (M.insert h1 e m1) (M.insert h2 e m2)


pickFirst :: [Maybe a] -> Maybe a
pickFirst (Just x:xs) = Just x
pickFirst (_:xs) = pickFirst xs
pickFirst _ = Nothing


lookup :: (MonadIO m, Hashable k) => k -> DMap a -> m (Maybe a)
lookup k (DMap m1 m2) = do
	(h1,h2) <- hashs k
	return $ pickFirst [M.lookup h1 m1, M.lookup h2 m2]


delete :: (MonadIO m, Hashable k) => k -> DMap a -> m (DMap a)
delete k (DMap m1 m2) = do
	(h1,h2) <- hashs k
	return $ DMap (M.delete h1 m1) (M.delete h2 m2)






