
-- tuple convertion and storable tuples

module Graphics.Farbe.Tuple where

import Foreign
import Control.Monad
import Data.List

err = error "unreachable value reached."


instance (Storable a, Storable b) => Storable (a,b) where
  sizeOf _
    = sizeOf (err :: a)
    + sizeOf (err :: b)
  alignment _ = maximum $ [alignment (err :: a), alignment (err :: b)]
  peek p = liftM2 (,)
    (peek $ castPtr p)
    (peekByteOff (castPtr p) (sizeOf (err :: a)))
  poke p (a,b) = do
    poke (castPtr p) a
    pokeByteOff (castPtr p) (sizeOf (err :: a)) b


instance (Storable a, Storable b, Storable c) => Storable (a,b,c) where
  sizeOf _
    = sizeOf (err :: a)
    + sizeOf (err :: b)
    + sizeOf (err :: c)
  alignment _ = maximum $ [alignment (err :: a), alignment (err :: b), alignment (err :: c)]
  peek p = liftM3 (,,)
    (peek $ castPtr p)
    (peekByteOff (castPtr p) (sizeOf (err :: a)))
    (peekByteOff (castPtr p) (sizeOf (err :: (a,b))))
  poke p (a,b,c) = do
    poke (castPtr p) a
    pokeByteOff (castPtr p) (sizeOf (err :: a)) b
    pokeByteOff (castPtr p) (sizeOf (err :: (a,b))) c


instance (Storable a, Storable b, Storable c, Storable d) => Storable (a,b,c,d) where
  sizeOf _
    = sizeOf (err :: a)
    + sizeOf (err :: b)
    + sizeOf (err :: c)
    + sizeOf (err :: d)
  alignment _ = maximum $ [alignment (err :: a), alignment (err :: b)
    , alignment (err :: c), alignment (err :: d)]
  peek p = liftM4 (,,,)
    (peek $ castPtr p)
    (peekByteOff (castPtr p) (sizeOf (err :: a)))
    (peekByteOff (castPtr p) (sizeOf (err :: (a,b))))
    (peekByteOff (castPtr p) (sizeOf (err :: (a,b,c))))
  poke p (a,b,c,d) = do
    poke (castPtr p) a
    pokeByteOff (castPtr p) (sizeOf (err :: a)) b
    pokeByteOff (castPtr p) (sizeOf (err :: (a,b))) c
    pokeByteOff (castPtr p) (sizeOf (err :: (a,b,c))) d





class Tuple1 a b where tfst :: a -> b
class Tuple2 a b where tsnd :: a -> b
class Tuple3 a b where ttrd :: a -> b
class Tuple4 a b where tth4 :: a -> b
-- ~ class Tuple5 a b | a -> b where t5 :: a -> b
-- ~ class Tuple6 a b | a -> b where t6 :: a -> b
-- ~ class Tuple7 a b | a -> b where t7 :: a -> b
-- ~ class Tuple8 a b | a -> b where t8 :: a -> b

instance Tuple1 (a,b)     a where tfst (a,b)     = a
instance Tuple1 (a,b,c)   a where tfst (a,b,c)   = a
instance Tuple1 (a,b,c,d) a where tfst (a,b,c,d) = a

instance Tuple2 (a,b)     b where tsnd (a,b)     = b
instance Tuple2 (a,b,c)   b where tsnd (a,b,c)   = b
instance Tuple2 (a,b,c,d) b where tsnd (a,b,c,d) = b

instance Tuple3 (a,b,c)   c where ttrd (a,b,c)   = c
instance Tuple3 (a,b,c,d) c where ttrd (a,b,c,d) = c

instance Tuple4 (a,b,c,d) d where tth4 (a,b,c,d) = d



tot1 = (++ "t1")
tot2 = (++ "t2")
tot3 = (++ "t3")
tot4 = (++ "t4")
