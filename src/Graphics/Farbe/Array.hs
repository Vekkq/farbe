{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.Array where

import Graphics.Farbe.Shader
import Graphics.Farbe.GL
import Graphics.Farbe.Vec (itoi)
import Graphics.Farbe.Tuple (err)


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Function
import Data.Foldable
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Data.Array.MArray as MA
import Numeric
import Foreign hiding (void)
import Foreign.C



-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

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
modifyArr a@(Arr _ sa) f = do
	i <- hashStableName <$> liftIO (makeStableName $ f sa)
	return $ Arr i sa

readArr :: forall m s a . (KnownNat s, MonadIO m, MArray StorableArray a m)
	=> Arr s a -> m [a]
readArr (Arr _ sa) = foldrMArray' (:) [] sa

instance Eq (Arr s a) where
	(Arr i _) == (Arr i2 _) = i == i2

instance (Storable e, KnownNat s) => Storable (Arr s e) where
	sizeOf a = sizeArr a * sizeOf (err :: e)
	alignment _ = alignment (err :: e)
	peek p = liftIO $ do
		ar <- emptyArr
		modifyArr ar (\sa -> withStorableArray sa $ \p2 -> copyArray (castPtr p) p2 (sizeArr ar))
	poke p a@(Arr _ sa) = withStorableArray sa $ \p2 -> copyArray p2 (castPtr p) (sizeArr a)
	-- i cant help but feel that this is borked


arr :: (GLtype a) => Expr e (Arr s a) -> Int32 -> Expr e a
arr e n = liftE2 "[]" e $ (expr n :: Expr e Int32)

-- | @arr'@ is ignoring constant expression requirement.
--   May not work with some implementations.
arr' :: (GLtype a) => Expr e (Arr s a) -> Expr e Int32 -> Expr e a
arr' = liftE2 "[]"

