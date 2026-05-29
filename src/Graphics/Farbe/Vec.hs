
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-|
Module      : Graphics.Farbe.Vec
Description : Algebraic vector math
Copyright   : (c) vekkq, 2024
License     : CC0
Maintainer  : vekkq@vivaldi.net
Stability   : experimental

A basic math module for vectors and matrices.
-}

module Graphics.Farbe.Vec
	-- * Vector types
	( V1 (..)
	, V2 (..)
	, V3 (..)
	, V4 (..)
	, Vector (..)
	-- * Vector operations
	, (*|)
	, (*||)
	, vdot
	, vlength
	, vdistance
	, vnormal
	, vcross
	, rel
	, relm
	, line
	, curve
	-- * Matrix operations
	, Mat
	, mtranspose
	, vtranspose
	, vtranspose'
	, mult
	, (****)
	, multv
	, (**|)
	, toList2
	-- * Rotation
	, rotate2D
	, rotationMatrix2D
	, rotate
	, roll
	, pitch
	, yaw
	, rotationMatrix
	-- * Perspective
	, perspective
	-- * Utility classes
	, FromList (..)
	, ToTuple (..)
	, FromTuple (..)
	, GetX (..)
	, GetY (..)
	, GetZ (..)
	, GetW (..)
	-- ~ , Fit (..)
	-- ~ , fit0
	, SizeUp (..)
	, SizeDown (..)
	-- * Miscellaneous
	, subSizeOf
	, itoi
	, for
	) where


import Graphics.Farbe.Tuple ()

import Control.Applicative
import Foreign.Storable
import Data.Foldable (toList)
import GHC.Generics (Generic)

import Foreign.Ptr



data V1 a = V1 a deriving (Read, Show, Eq, Ord, Generic)

instance Functor V1 where
  fmap f (V1 x) = V1 (f x)

instance Applicative V1 where
  pure a = V1 a
  (V1 f) <*> (V1 x) = V1 (f x)

instance Foldable V1 where
  foldMap f (V1 x) = f x

instance Traversable V1 where
  sequenceA (V1 x) = V1 <$> x


data V2 a = V2 a a deriving (Read, Show, Eq, Ord, Generic)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
  pure a = V2 a a
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

instance Traversable V2 where
  sequenceA (V2 x y) = liftA2 V2 x y


data V3 a = V3 a a a deriving (Read, Show, Eq, Ord, Generic)

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Applicative V3 where
  pure a = V3 a a a
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)

instance Foldable V3 where
  foldMap f (V3 x y z) = f x <> f y <> f z

instance Traversable V3 where
  sequenceA (V3 x y z) = liftA3 V3 x y z


data V4 a = V4 a a a a deriving (Read, Show, Eq, Ord, Generic)

instance Functor V4 where
  fmap f (V4 x y z w) = V4 (f x) (f y) (f z) (f w)

instance Applicative V4 where
  pure a = V4 a a a a
  (V4 f g h i) <*> (V4 x y z w) = V4 (f x) (g y) (h z) (i w)

instance Foldable V4 where
  foldMap f (V4 x y z w) = f x <> f y <> f z <> f w

instance Traversable V4 where
  sequenceA (V4 x y z w) = V4 <$> x <*> y <*> z <*> w

-- | Unifying vector class.
class (Applicative v, Foldable v, Traversable v, FromList v) => Vector v where
  vsize :: Num n => v a -> n

instance Vector V1 where
  vsize _ = 1

instance Vector V2 where
  vsize _ = 2

instance Vector V3 where
  vsize _ = 3

instance Vector V4 where
  vsize _ = 4



#define INSTANCENUM(vn)                   \
instance Num a => Num (vn a) where      { \
  v + v2 = pure (+) <*> v <*> v2        ; \
  v - v2 = pure (-) <*> v <*> v2        ; \
  v * v2 = pure (*) <*> v <*> v2        ; \
  signum v = pure signum <*> v          ; \
  abs v = pure abs <*> v                ; \
  fromInteger i = pure (fromInteger i)  ; \
                                        } \

INSTANCENUM(V1)
INSTANCENUM(V2)
INSTANCENUM(V3)
INSTANCENUM(V4)


#define INSTANCEFRAC(vn)                            \
instance Fractional a => Fractional (vn a) where  { \
  v / v2 = pure (/) <*> v <*> v2                  ; \
  recip v = pure recip <*> v                      ; \
  fromRational r = pure (fromRational r)          ; \
                                                  } \

INSTANCEFRAC(V1)
INSTANCEFRAC(V2)
INSTANCEFRAC(V3)
INSTANCEFRAC(V4)



-- | Scalar multiplication.
(*|) :: (Vector v, Num (v a)) => a -> v a -> v a
a *| v = pure a * v

-- | Scalar multiplication using a matrix.
(*||) :: (Vector x, Vector y, Num (x (y a))) => a -> Mat x y a -> Mat x y a
a *|| v = pure (pure a) * v


-- | Dot product.
vdot :: (Vector v, Num (v a), Num a) => v a -> v a -> a
vdot v1 v2 = sum $ v1*v2

-- | Vector length.
vlength :: (Vector v, Num (v a), Floating a) => v a -> a
vlength v = sqrt $ vdot v v


-- | Distance between two vector coordinates.
vdistance :: (Vector v, Num (v a), Floating a) => v a -> v a -> a
vdistance a b = vlength (a-b)

-- ~ -- | Turning a vector by 90°.
-- ~ orth :: Num a => V2 a -> V2 a
-- ~ orth v = V2 negate id <*> flipVec v

-- ~ -- | Swap x with y of a vector.
-- ~ flipVec :: V2 a -> V2 a
-- ~ flipVec (V2 x y) = V2 y x

-- | Normalize vectors. Reduce a vector's length to 1, by retaining orientation.
vnormal :: (Vector v, Fractional (v a), Floating a) => v a -> v a
vnormal v = v / (pure $ vlength v)

-- | Cross product.
vcross :: Num a => V3 a -> V3 a -> V3 a
vcross (V3 x y z) (V3 x2 y2 z2) = V3 (y*z2 - y2*z) (z*x2 - z2*x) (x*y2 - x2*y)

-- | The first parameter is substracted and then added back afterwards.
rel :: Num v => v -> (v -> v) -> v -> v
rel r f = (+r) . f . subtract r

-- | Same as @rel@ for multiplication.
relm :: Fractional v => v -> (v -> v) -> v -> v
relm r f = (*r) . f . (/r)

-- | Calculates a point on a line between its two parameters. The point is based on the third parameter, which is interpolated by values between 0 and 1 .
line :: (Vector v, Num (v a)) => v a -> v a -> a -> v a
line v v2 t = rel v (* pure t) v2

-- | Bezier curve with one control point.
curve :: (Vector v, Num (v a)) => v a -> v a -> v a -> a -> v a
curve v v2 v3 t = line (line v v2 t) (line v2 v3 t) t





-- ~ -- angle-optimized curve
-- ~ acurve :: (Vector v, Num a) => a -> v a -> v a -> v a -> [v a]
-- ~ acurve = undefined

-- ~ vcross' :: (Vector v, Num a) => v a -> v a -> v a
-- ~ vcross' v1 v2 = vcross (fromList v1) (fromList v2)




-- | Matrix type.
type Mat vx vy a = vx (vy a)

-- | Matrix transpose.
mtranspose :: (Vector x, Vector y) => Mat x y a -> Mat y x a
mtranspose = sequenceA

-- | Tranpose vertex to matrix.
vtranspose :: Vector v => v a -> Mat v V1 a
vtranspose v = fmap V1 v

-- | Tranpose matrix to vertex.
vtranspose' :: Vector v => Mat v V1 a -> v a
vtranspose' v = fmap f v
  where f (V1 x) = x

-- | Matrix multiplication.
mult :: (Vector m, Vector n, Vector h, Num (m a), Num (n a), Num (h a), Num a) => Mat m n a -> Mat n h a -> Mat m h a
mult a b = traverse (traverse vdot a) $ mtranspose b

-- | Matrix multiplication, alias.
(****) :: (Vector m, Vector n, Vector h, Num (m a), Num (n a), Num (h a), Num a) => Mat m n a -> Mat n h a -> Mat m h a
(****) = mult

-- | Matrix multiplication with vector.
multv :: (Vector m, Vector n, Num (m a), Num (n a), Num a) => Mat m n a -> n a -> m a
multv a b = vtranspose' $ a **** vtranspose b

-- | Matrix multiplication with vector, alias.
(**|) :: (Vector m, Vector n, Num (m a), Num (n a), Num a) => Mat m n a -> n a -> m a
(**|) = multv

toList2 :: (Foldable f, Foldable g) => f (g a) -> [a]
toList2 = concatMap toList . toList



-- | Rotate in 2D space.
rotate2D :: Floating a => a -> V2 a -> V2 a
rotate2D a v = rotationMatrix2D a **| v

-- | Define a multiplication matrix for 2D space.
rotationMatrix2D :: Floating a => a -> Mat V2 V2 a
rotationMatrix2D a = V2 (V2 (cos a) (negate $ sin a)) (V2 (sin a) (cos a))

-- | Rotate in 3D space.
rotate :: Floating a => a -> a -> a -> V3 a -> V3 a
rotate a b c v = rotationMatrix a b c **| v

roll, pitch, yaw :: Floating a => a -> Mat V3 V3 a
roll a = V3 (V3 (cos a) (-sin a) 0) (V3 (sin a) (cos a) 0) (V3 0 0 1)
pitch a = V3 (V3 (cos a) 0 (sin a)) (V3 0 1 0) (V3 (-sin a) 0 (cos a))
yaw a = V3 (V3 1 0 0) (V3 0 (cos a) (-sin a)) (V3 0 (sin a) (cos a))

-- | Define a multiplication matrix for 3D space.
rotationMatrix :: Floating a => a -> a -> a -> Mat V3 V3 a
rotationMatrix a b c = roll a **** pitch b **** yaw c


-- | Multiplication matrix for orthogonal to perspective projection.
perspective
  :: Floating a
  => a -- ^ FOV (y direction, in radians)
  -> a -- ^ Aspect ratio
  -> a -- ^ Near plane
  -> a -- ^ Far plane
  -> Mat V4 V4 a
perspective fovy aspect near far =
  V4 (V4 x 0 0    0)
     (V4 0 y 0    0)
     (V4 0 0 z    w)
     (V4 0 0 (-1) 0)
  where tanHalfFovy = tan $ fovy / 2
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        fpn = far + near
        fmn = far - near
        oon = 0.5/near
        oof = 0.5/far
        -- z = 1 / (near/fpn - far/fpn) -- would be better by .5 bits
        z = -fpn/fmn
        w = 1/(oof-oon) -- 13 bits error reduced to 0.17
        -- w = -(2 * far * near) / fmn
-- copied from linear





class ToTuple a b where toTuple :: a -> b
instance ToTuple (V2 a) (a,a) where toTuple (V2 x y) = (x,y)
instance ToTuple (V3 a) (a,a,a) where toTuple (V3 x y z) = (x,y,z)
instance ToTuple (V4 a) (a,a,a,a) where toTuple (V4 x y z w) = (x,y,z,w)

class FromTuple a b where fromTuple :: a -> b
instance FromTuple (a,a) (V2 a) where fromTuple (x,y) = V2 x y
instance FromTuple (a,a,a) (V3 a) where fromTuple (x,y,z) = V3 x y z
instance FromTuple (a,a,a,a) (V4 a) where fromTuple (x,y,z,w) = V4 x y z w


class GetX v where getx :: v a -> a
instance GetX V1 where getx (V1 x) = x
instance GetX V2 where getx (V2 x _) = x
instance GetX V3 where getx (V3 x _ _) = x
instance GetX V4 where getx (V4 x _ _ _) = x

class GetY v where gety :: v a -> a
instance GetY V2 where gety (V2 _ y) = y
instance GetY V3 where gety (V3 _ y _) = y
instance GetY V4 where gety (V4 _ y _ _) = y

class GetZ v where getz :: v a -> a
instance GetZ V3 where getz (V3 _ _ z) = z
instance GetZ V4 where getz (V4 _ _ z _) = z

class GetW v where getw :: v a -> a
instance GetW V4 where getw (V4 _ _ _ w) = w



class FromList t where
  fromListFill :: a -> [a] -> t a

instance FromList V1 where
  fromListFill a xs | (x:_) <- xs ++ repeat a = V1 x

instance FromList V2 where
  fromListFill a xs | (x:y:_) <- xs ++ repeat a = V2 x y

instance FromList V3 where
  fromListFill a xs | (x:y:z:_) <- xs ++ repeat a = V3 x y z

instance FromList V4 where
  fromListFill a xs | (x:y:z:w:_) <- xs ++ repeat a = V4 x y z w

fromList :: (FromList t, Num a) => [a] -> t a
fromList = fromListFill 0


class Fit x a b where fit :: x -> a -> b

instance Fit a (V1 a) (V2 a) where fit a (V1 x) = V2 x a
instance Fit a (V2 a) (V3 a) where fit a (V2 x y) = V3 x y a
instance Fit a (V3 a) (V4 a) where fit a (V3 x y z) = V4 x y z a

instance Fit a (V4 a) (V3 a) where fit a (V4 x y z w) = V3 x y z
instance Fit a (V3 a) (V2 a) where fit a (V3 x y z) = V2 x y
instance Fit a (V2 a) (V1 a) where fit a (V2 x y) = V1 x

instance Fit a (V1 a) (V1 a) where fit a = id
instance Fit a (V2 a) (V2 a) where fit a = id
instance Fit a (V3 a) (V3 a) where fit a = id
instance Fit a (V4 a) (V4 a) where fit a = id

-- ~ instance {-# OVERLAPPING #-} Fit a v1 v2 where fit a v = fit a $ fit a v

fit0 :: (Num a, Fit a (v a) (v2 a)) => v a -> v2 a
fit0 = fit 0



-- | Class to size __up__ vector to asked size.
class SizeUp x a b | a -> x, b -> x where up :: x -> a -> b
instance SizeUp a (V2 a) (V3 a) where up i (V2 x y) = V3 x y i
instance SizeUp a (V2 a) (V4 a) where up i (V2 x y) = V4 x y i i
instance SizeUp a (V3 a) (V4 a) where up i (V3 x y z) = V4 x y z i

-- | Class to size __down__ vector to asked size.
class SizeDown a b where down :: a -> b
instance SizeDown (V3 a) (V2 a) where down (V3 x y _)   = (V2 x y)
instance SizeDown (V4 a) (V2 a) where down (V4 x y _ _) = (V2 x y)
instance SizeDown (V4 a) (V3 a) where down (V4 x y z _) = (V3 x y z)



#define bottom undefined

-- | sizeOf variant for type parameter.
subSizeOf :: forall g a n. (Storable a, Num n) => g a -> n
subSizeOf _ = itoi $ sizeOf (bottom :: a)

itoi :: (Integral a, Num c) => a -> c
itoi = fromInteger . toInteger


-- | @map@ with flipped parameters.
for :: [a] -> (a -> b) -> [b]
for = flip map


instance Storable a => Storable (V1 a) where
  sizeOf = subSizeOf
  alignment = subSizeOf
  peek p = V1 <$> peek (castPtr p)
  poke p (V1 x) = poke (castPtr p) x

instance Storable a => Storable (V2 a) where
  sizeOf = (2*) . subSizeOf
  alignment = subSizeOf
  peek p = (fromTuple :: (a,a) -> (V2 a)) <$> peek (castPtr p)
  poke p v = poke (castPtr p) $ (toTuple :: (V2 a) -> (a,a)) v

instance Storable a => Storable (V3 a) where
  sizeOf = (3*) . subSizeOf
  alignment = subSizeOf
  peek p = (fromTuple :: (a,a,a) -> (V3 a)) <$> peek (castPtr p)
  poke p v = poke (castPtr p) $ (toTuple :: (V3 a) -> (a,a,a)) v

instance Storable a => Storable (V4 a) where
  sizeOf = (4*) . subSizeOf
  alignment = subSizeOf
  peek p = (fromTuple :: (a,a,a,a) -> (V4 a)) <$> peek (castPtr p)
  poke p v = poke (castPtr p) $ (toTuple :: (V4 a) -> (a,a,a,a)) v





