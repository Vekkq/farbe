{-# OPTIONS_GHC -fno-warn-tabs #-}
-- ~ {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.OBJ where

import Graphics.Farbe
import Graphics.Farbe.Attribute
import Control.Monad
import Foreign.Storable
import Foreign.Ptr

import Codec.Wavefront
import Data.Vector ((!?), (!))
import Data.Foldable
import Data.Maybe

import Debug.Trace

#define bottom undefined



data OBJPoint = OBJPoint
	{ oCoord :: V3 Float
	, oTexco :: V3 Float
	, oNormal :: V3 Float
	} deriving (Read, Show, Eq)


instance Storable OBJPoint where
  sizeOf _ = (3*) $ subSizeOf (bottom :: V3 Float)
  alignment _ = alignment (bottom :: V3 Float)
  peek p = uncurry3 OBJPoint <$> peek (castPtr p)
  poke p (OBJPoint o n t) = poke (castPtr p) (o, n, t)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

data OBJPointE = OBJPointE
	{ coord :: V3 (Expr V Float)
	, normal :: V3 (Expr V Float)
	, texco :: V3 (Expr V Float)
	}

instance AttrType OBJPoint OBJPointE where
	setAttribute _ = liftM3 OBJPointE
		(setAttribute (bottom :: V3 Float))
		(setAttribute (bottom :: V3 Float))
		(setAttribute (bottom :: V3 Float))


loadOBJ :: Farbe m => FilePath -> m [OBJPoint]
loadOBJ s = fromFile s >>= either error (return . f)
	where
		f :: WavefrontOBJ -> [OBJPoint]
		f wave = concatMap (fromFace wave . elValue) $ toList $ objFaces $ wave


fromFace :: WavefrontOBJ -> Face -> [OBJPoint]
fromFace wave (Face i j k xs) = let
	ys = triangulize (i:j:k:xs)
	tris@(z:_) = map2 (fromFaceIndex wave) ys
	in if V3 0 0 0 == (oNormal $ getx z)
	then concatMap (toList . f) tris
	else concatMap toList tris
	where
		f :: V3 OBJPoint -> V3 OBJPoint
		f v3objp = fmap (addCalculatedNormal (fmap oCoord v3objp)) v3objp

map2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
map2 = (fmap . fmap)

triangulize :: [FaceIndex] -> [V3 FaceIndex]
triangulize (a:b:c:xs) = V3 a b c : triangulize (a:c:xs)
triangulize _ = []


fromFaceIndex :: WavefrontOBJ -> FaceIndex -> OBJPoint
fromFaceIndex wave (FaceIndex ic mit min) = let
	c = maybe (V3 0 0 0) lToVec $ objLocations wave !? (pred ic)
	t = maybe (V3 0 0 0) tToVec $ objTexCoords wave !?! (pred <$> mit)
	n = maybe (V3 0 0 0) nToVec $ objNormals wave !?! (pred <$> min)
	in OBJPoint c t n


v !?! (Just i) = v !? i
v !?! Nothing = Nothing

addCalculatedNormal :: V3 (V3 Float) -> OBJPoint -> OBJPoint
addCalculatedNormal (V3 v1 v2 v3) op = op {
		oNormal = vcross (v2 - v1) (v3 - v1)
	}

m000 = fromMaybe (V3 0 0 0)

lToVec (Location f1 f2 f3 f4) = V3 f1 f2 f3
tToVec (TexCoord f1 f2 f3) = V3 f1 f2 f3
nToVec (Normal f1 f2 f3) = V3 f1 f2 f3

