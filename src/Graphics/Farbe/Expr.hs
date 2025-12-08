{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Graphics.Farbe.Expr where

import Graphics.Farbe.Shader
import Graphics.Farbe.GL
import Graphics.Farbe.Vec
import Graphics.Farbe.Texture
import Graphics.Farbe.Array


import Numeric
import Foreign hiding (void)


instance (GLtype a, Num a) => Num (Expr e a) where
	(+) = liftE2 "+"
	(*) = liftE2 "*"
	(-) = liftE2 "-"
	abs = liftE1 "abs"
	signum = liftE1 "sign"
	fromInteger = liftE0 . ($ "") . showFFloat Nothing . fromInteger

instance (GLtype a, Fractional a) => Fractional (Expr e a) where
	fromRational = liftE0 . ($ "") . showFFloat Nothing . fromRat
	(/) = liftE2 "/"

napier :: Fractional a => a
napier = fromRational 2.718281828459045235360287471352

-- | Unicode alias for Napier's constant
e :: Fractional a => a
e = napier

instance (GLtype a, Floating a) => Floating (Expr e a) where
	pi = liftE0 $ show pi
	exp = liftE1 "exp"
	log = liftE1 "log"
	sqrt = liftE1 "sqrt"
	(**) = liftE2 "^"
	sin = liftE1 "sin"
	cos = liftE1 "cos"
	tan = liftE1 "tan"
	asin = liftE1 "asin"
	acos = liftE1 "acos"
	atan = liftE1 "atan"
	-- following functions not available in glsl es 1
	sinh x = (e ** x - e ** (negate x)) / 2
	cosh x = (e ** x + e ** (negate x)) / 2
	tanh x = sinh x / cosh x
	asinh x = ln (x + sqrt (x**2 + 1))
	acosh x = ln (x + sqrt (x**2 - 1))
	atanh x = 1/2 * ln ((1+x) / (1-x))

ln :: Floating a => a -> a
ln = logBase e

modf :: Expr e Float -> Expr e Float -> Expr e Float
modf = liftE2 "mod"

equot, erem, ediv, emod :: Expr e Int32 -> Expr e Int32 -> Expr e Int32
equot = liftE2 "/"
erem = liftE2 "rem"
ediv = liftE2 "div"
emod = liftE2 "mod"

-- TODO add non-component-wise vector and matrix functions

fragCoord :: V4 (Expr F Float)
fragCoord = vecParts $ liftE0 "gl_FragCoord"

texture :: Expr e (Texture f) -> V2 (Expr e Float) -> V4 (Expr e Float)
texture t v = vecParts $ liftE2 "texture2D" t (exprVec v)

arr :: GLtype a => Expr e (Arr s a) -> Int32 -> Expr e a
arr e' n = liftE2 "[]" e' $ (expr n :: Expr e Int32)

-- | @arr'@ is ignoring constant expression requirement.
--   May not work with some implementations.
arr' :: GLtype a => Expr e (Arr s a) -> Expr e Int32 -> Expr e a
arr' = liftE2 "[]"

if' :: GLtype a => Expr e Bool -> Expr e a -> Expr e a -> Expr e a
if' = liftE3 "if"
