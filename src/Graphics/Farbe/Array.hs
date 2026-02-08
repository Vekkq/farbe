{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Array where

-- ~ import Graphics.Farbe.Shader
import Graphics.Farbe.Vec (itoi)
-- ~ import Graphics.Farbe.Tuple (err)


import Data.Array.IO
import Data.Array.Storable
import Foreign hiding (void)


import Control.Monad.Reader

import GHC.TypeNats
import Data.Proxy

import System.Mem.StableName


-- Array stub ----------------------------------------------------------------------------


data Arr (s :: Nat) a = Arr
	{ changeToken :: Int
	, unArr :: StorableArray Int a
	}

sizeArr :: forall n s a . (KnownNat s, Num n) => (Arr s a) -> n
sizeArr _ = itoi (natVal (Proxy :: Proxy s))

sizeArr' :: forall n s a p . (KnownNat s, Num n) => p (Arr s a) -> n
sizeArr' _ = itoi (natVal (Proxy :: Proxy s))

newArr :: forall m s a . (KnownNat s, Storable a, MonadIO m) => [a] -> m (Arr s a)
newArr l = liftIO $ Arr 0 <$> newListArray (0, pred $ itoi (natVal (Proxy :: Proxy s))) l

emptyArr :: forall m s a . (KnownNat s, Storable a, MonadIO m) => m (Arr s a)
emptyArr = liftIO $ Arr 0 <$> newArray_ (0, pred $ itoi (natVal (Proxy :: Proxy s)))

modifyArr :: MonadIO m => Arr s a -> (StorableArray Int a -> m b) -> m (Arr s a)
modifyArr (Arr _ sa) f = do
	i <- hashStableName <$> liftIO (makeStableName $ f sa)
	return $ Arr i sa

readArr :: forall m s a . (KnownNat s, MonadIO m, MArray StorableArray a m)
	=> Arr s a -> m [a]
readArr (Arr _ sa) = foldrMArray' (:) [] sa

instance Eq (Arr s a) where
	(Arr i _) == (Arr i2 _) = i == i2


#define bottom undefined

instance (Storable e, KnownNat s) => Storable (Arr s e) where
	sizeOf a = sizeArr a * sizeOf (bottom :: e)
	alignment _ = alignment (bottom :: e)
	peek p = liftIO $ do
		ar <- emptyArr
		modifyArr ar (\sa -> withStorableArray sa $ \p2 -> copyArray (castPtr p) p2 (sizeArr ar))
	poke p a@(Arr _ sa) = withStorableArray sa $ \p2 -> copyArray p2 (castPtr p) (sizeArr a)
	-- i cant help but feel that this is borked
