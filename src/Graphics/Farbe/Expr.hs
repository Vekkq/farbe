{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphics.Farbe.Expr where

import Graphics.Farbe.GL
import Graphics.Farbe.Vec
import Graphics.Farbe.Texture
import Graphics.Farbe.Array
import Data.Foldable
import Data.Hashable
import GHC.Generics (Generic)
-- ~ import Graphics.Farbe.BuildShader

import Numeric
import Foreign hiding (void)

#define bottom undefined

-- Expr ----------------------------------------------------------------------------------

-- | Expression type to define shader definitions.
data Expr e a = Expr { unExpr :: ExprI } deriving (Eq, Ord, Show, Read, Generic, Hashable)

data ExprI = ExprI
	{ fnName :: String, rtype :: TypeS, fnAst :: [ExprI], exprSetup :: Register }
	deriving (Eq, Ord, Show, Read, Generic, Hashable)

data Register = RegisterNone | RegisterVarying | RegisterUniform | RegisterVertex
	deriving (Eq, Ord, Show, Read, Generic, Hashable)

data F
data V

-- Shader result types. Results for Vertex & Fragment shader respectively.
type SResult = (V4 (Expr V Float), V4 (Expr F Float))

liftExpr :: forall a e . GLtype a => String -> [ExprI] -> Expr e a
liftExpr s p = Expr $ ExprI s (toTypeS (bottom :: a)) p RegisterNone

liftE0 :: GLtype a => String -> Expr e a
liftE0 s = liftExpr s []

liftE1 :: (GLtype a2) => String -> Expr e a1 -> Expr e a2
liftE1 s (Expr a) = liftExpr s [a]

liftE2 :: (GLtype a3) => String -> Expr e a1 -> Expr e a2 -> Expr e a3
liftE2 s (Expr a) (Expr b) = liftExpr s [a,b]

liftE3 :: (GLtype a4) => String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
liftE3 s (Expr a) (Expr b) (Expr c) = liftExpr s [a,b,c]

-- ~ freeEnv :: Expr A a -> Expr e a
-- ~ freeEnv (Expr e) = (Expr e)

crawl :: (ExprI -> a) -> ExprI -> [a]
crawl f e@(ExprI _ _ ps _) = f e : concatMap (crawl f) ps

mapExpr :: Monad m => (ExprI -> m ExprI) -> ExprI -> m ExprI
mapExpr f e = do
	g <- f e
	ps <- mapM (mapExpr f) $ fnAst g
	return $ g { fnAst = ps }


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

-- | e
napier :: Fractional a => a
napier = fromRational 2.718281828459045235360287471352

instance (GLtype a, Floating a) => Floating (Expr e a) where
	pi = liftE0 $ show pi
	exp = liftE1 "exp"
	log = liftE1 "log"
	sqrt = liftE1 "sqrt"
	(**) = liftE2 "pow"
	sin = liftE1 "sin"
	cos = liftE1 "cos"
	tan = liftE1 "tan"
	asin = liftE1 "asin"
	acos = liftE1 "acos"
	atan = liftE1 "atan"
	-- following functions are not available in glsl es 1
	sinh x = (napier ** x - napier ** (negate x)) / 2
	cosh x = (napier ** x + napier ** (negate x)) / 2
	tanh x = sinh x / cosh x
	asinh x = ln (x + sqrt (x**2 + 1))
	acosh x = ln (x + sqrt (x**2 - 1))
	atanh x = 1/2 * ln ((1+x) / (1-x))

ln :: Floating a => a -> a
ln = logBase napier

-- | modulo on float.
modf, log2 :: Expr e Float -> Expr e Float -> Expr e Float
modf = liftE2 "mod"

log2 = liftE2 "log2"

efloor :: Expr e Float -> Expr e Float
efloor = liftE1 "floor"

-- | Identical to quot, but in Expr space.
equot, erem, ediv, emod :: Expr e Int32 -> Expr e Int32 -> Expr e Int32
equot = liftE2 "/"
erem = liftE2 "rem"
ediv = liftE2 "div"
emod = liftE2 "mod"


vecParts :: (GLtype a, Vector v) => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill bottom $ map (\i -> arrV e i) $ map literal [0..]

matParts :: (GLtype a, GLtype (v a), Vector v) => Expr e (v (v a)) -> v (v (Expr e a))
matParts = fmap vecParts . vecParts

exprVec :: forall e a v . (GLtype a, Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = liftExpr (slName (bottom :: v a)) $ map unExpr $ toList v

exprMat :: forall a e v .(GLtype a, Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr e a)) -> Expr e (v (v a))
exprMat v = liftExpr (slName (bottom :: v a)) $ map unExpr $ concatMap toList $ toList v

arrV :: (GLtype a, Vector v) => Expr e (v a) -> Expr e Int32 -> Expr e a
arrV = liftE2 "[]"

literal :: (Show b, GLtype a) => b -> Expr e a
literal x = liftE0 $ show x


-- | position coordinate in Fragment shader.
fragCoord :: V4 (Expr F Float)
fragCoord = vecParts $ liftE0 "gl_FragCoord"

-- | Access values of a texture based on given coordinates.
texture :: Expr e Texture -> V2 (Expr e Float) -> V4 (Expr e Float)
texture t v = texture' t (mapy (1-) v)


-- | Original unflipped texture variables.
texture' :: Expr e Texture -> V2 (Expr e Float) -> V4 (Expr e Float)
texture' t v = vecParts $ liftE2 "texture2D" t (exprVec v)


arr :: GLtype a => Expr e (Arr s a) -> Int32 -> Expr e a
arr e' n = liftE2 "[]" e' $ (literal n :: Expr e Int32)

-- | @arr'@ is ignoring constant expression requirement.
--   May not work with some implementations.
arr' :: GLtype a => Expr e (Arr s a) -> Expr e Int32 -> Expr e a
arr' = liftE2 "[]"

-- | Expr space @if@.
if' :: GLtype a => Expr e Bool -> Expr e a -> Expr e a -> Expr e a
if' = liftE3 "if"

-- | Transmitting values from vertex to fragment shader. If a value is transferred from the rendered vertex array directly, they are interpolated to their coordinate in fragment space.
class Transfer a b | a -> b, b -> a where
	transferFrag :: a -> b

instance Transfer (Expr V Float) (Expr F Float) where
	transferFrag (Expr e) = Expr $ ExprI "transferFrag" TFloat [e] RegisterVarying

instance Transfer (Expr V Int) (Expr F Int) where
	transferFrag (Expr e) = Expr $ ExprI "transferFrag" TInt [e] RegisterVarying

instance Transfer (Expr V Bool) (Expr F Bool) where
	transferFrag (Expr e) = Expr $ ExprI "transferFrag" TBool [e] RegisterVarying

instance (Transfer a b) => Transfer (V2 a) (V2 b) where
	transferFrag = fmap transferFrag

instance (Transfer a b) => Transfer (V3 a) (V3 b) where
	transferFrag = fmap transferFrag

instance (Transfer a b) => Transfer (V4 a) (V4 b) where
	transferFrag = fmap transferFrag

