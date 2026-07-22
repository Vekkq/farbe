{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Shader2 where

import Graphics.Farbe.Vec
import Graphics.Farbe.GL
-- ~ import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
-- ~ import Graphics.Farbe.State
-- ~ import Graphics.Farbe.BuildShader
-- ~ import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Utility
-- ~ import Graphics.Farbe.Expr

import Data.Char
import Data.List
import Data.Foldable
import Data.Hashable
import Foreign hiding (void)
import Foreign.C
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.IntMap as M
import qualified Data.Set as S

import Data.Dynamic
import Numeric

import Graphics.GL.Embedded20
import Graphics.GL.Types
import Graphics.GL.Ext.OES.VertexArrayObject

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

#define bottom undefined


-- ShdrState -----------------------------------------------------------------------------

data ShdrState = ShdrState { shdrMap :: M.IntMap Dynamic }

class Monad m => HandShdr m where
	stateShdr :: (ShdrState -> (a, ShdrState)) -> m a

	-- ~ delayShdr :: m ((ShdrState -> ShdrState) -> IO ())

getShdr :: HandShdr m => m ShdrState
getShdr = stateShdr (\s -> (s, s))

setShdr :: HandShdr m => ShdrState -> m ()
setShdr s = stateShdr (\_ -> ((), s))



-- Expr ----------------------------------------------------------------------------------

data Expr e a = Expr { unExpr :: ExprI } deriving Functor

data ExprI = ExprI
	{ fnName :: String, rtype :: TypeS, fnAst :: [ExprI], exprSetup :: Register }

data Register = None | RegisterUniform | RegisterVertex | RegisterVarying | RegisterOut

liftExpr :: forall a e . (GLtype a) => String -> [ExprI] -> Expr e a
liftExpr s p = Expr $ ExprI s (toTypeS (bottom :: a)) p None


liftE0 ::(GLtype a) => String -> Expr e a
liftE0 s = liftExpr s []

liftE1 :: (GLtype a2) => String -> Expr e a1 -> Expr e a2
liftE1 s (Expr a) = liftExpr s [a]

liftE2 :: (GLtype a3) => String -> Expr e a1 -> Expr e a2 -> Expr e a3
liftE2 s (Expr a) (Expr b) = liftExpr s [a,b]

liftE3 :: (GLtype a4) => String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
liftE3 s (Expr a) (Expr b) (Expr c) = liftExpr s [a,b,c]


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

modf, log2 :: Expr e Float -> Expr e Float -> Expr e Float
modf = liftE2 "mod"
log2 = liftE2 "log2"

efloor :: Expr e Float -> Expr e Float
efloor = liftE1 "floor"


equot, erem, ediv, emod :: Expr e Int32 -> Expr e Int32 -> Expr e Int32
equot = liftE2 "/"
erem = liftE2 "rem"
ediv = liftE2 "div"
emod = liftE2 "mod"








-- SHADER DEFINITION ---------------------------------------------------------------------

class Shader f g where
	shader :: f -> g
	shader = undefined

data F
data V

-- ~ instance Shader f g => Shader (Expr V Int -> f) (Int -> g)

instance Shader f g => Shader (Mat V3 V3 (Expr V Float) -> f) (Mat V3 V3 Float -> g)


instance (Shader f g) => Shader
	((V3 (Expr V Float), V3 (Expr V Float)) -> f)
	(VArray (V3 Float, V3 Float) -> g)

-- ~ instance (Shader f g, AttrType a b) => Shader
	-- ~ (b -> f)
	-- ~ (VArray a -> g)




instance (HandShdr m) => Shader
	(V4 (Expr V Float), V4 (Expr F Float))
	(m Bool)


-- ~ foo :: Shader
	-- ~ ((V3 (Expr V Float), V3 (Expr V Float)) -> (V4 (Expr V Float), V4 (Expr F Float)))
	-- ~ (VArray (V3 Float, V3 Float) -> m Bool)

-- VAO LAND ------------------------------------------------------------------------------

glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = glBindVertexArrayOES

type ShaderId = GLuint

data ShaderType = Vertex | Fragment

data ShaderData = ShaderData
	{ shaderId :: ShaderId
	, vaoByteCount :: Int
	, vaoByteMax :: Int
	, headers :: S.Set (ShaderType, Header)
	, exprs :: S.Set (ShaderType, ExprI)
	}

emptyShaderData :: MonadIO m => m ShaderData
emptyShaderData = do
	i<- glCreateProgram
	return $ ShaderData
		{ shaderId = i
		, vaoByteCount = 0
		, vaoByteMax = error "not defined"
		, headers = S.empty
		, exprs = S.empty
		}

newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT ShaderData m a }
	deriving
		(Functor, Applicative, Monad, MonadIO)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Functor m) => ShaderEnv m where
	stateShader :: (ShaderData -> (a, ShaderData)) -> m a

modifyShader :: ShaderEnv m => (ShaderData -> ShaderData) -> m ()
modifyShader f = stateShader (\s -> ((), f s))

getsShader :: ShaderEnv m => (ShaderData -> r) -> m r
getsShader f = f <$> getShader

getShader :: ShaderEnv m => m (ShaderData)
getShader = stateShader (\s -> (s, s))

putShader :: ShaderEnv m => ShaderData -> m ()
putShader s = stateShader (\_ -> ((),s))

instance Monad m => ShaderEnv (ShaderEnvT m) where
	stateShader = ShaderEnvT . state

runShaderEnvT :: (MonadIO m) => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = emptyShaderData >>= runStateT ms

setByteMax :: (ShaderEnv m) => Int -> m ()
setByteMax i = modifyShader $ \s -> s { vaoByteMax = i }


data Header

type Vao = GLuint

-- | Make VAO
setAttributes :: (MonadIO m, AttrType a b, ShaderEnv m) => a -> m (Vao, b)
setAttributes a = do
	i <- glGenVertexArray
	glBindVertexArray i
	setByteMax (sizeOf a)
	e <- setAttribute a
	return (i, e)


class Storable a => AttrType a b | a -> b, b -> a where
	setAttribute :: (ShaderEnv m, MonadIO m) => a -> m b

setupAttribute1
	:: (ShaderEnv m, Monad m, GLtype a, Storable a) => a -> m (Expr V a)
setupAttribute1 a = undefined
-- ~ do
	-- ~ s <- getShaderId
	-- ~ n <- name "a" a
	-- ~ maxSize <- getByteMax
	-- ~ o <- advanceBy a
	-- ~ postShader $ withString n $ \c -> do
		-- ~ p <- fromIntegral <$> glGetAttribLocation s c
		-- ~ when (p < 2^15-1) $ do
			-- ~ glVertexAttribPointer p
				-- ~ (glComponents a)
				-- ~ (glType a)
				-- ~ (glNormalized a)
				-- ~ (itoi $ maxSize)
				-- ~ (intPtrToPtr $ IntPtr o)
			-- ~ glEnableVertexAttribArray p
		-- ~ liftIO $ putStrLn $ "sl pos: " ++ show p ++ "\t arr pos: " ++ show o ++ "\t stride: " ++ (show $ itoi $ maxSize - sizeOf a) ++ "\t components: " ++ (show $ glComponents a)
	-- ~ return $ liftExprShdr' $ do
		-- ~ addHeader "attribute" a n
		-- ~ return n


instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

-- not working that well and probably not necessary
-- ~ instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	-- ~ setAttribute a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 a
		-- ~ where
		-- ~ fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
		-- ~ fmap2 f = fmap (fmap f)


instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute _ = liftM2 (,) (setAttribute (bottom :: a)) (setAttribute (bottom :: b))

instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	setAttribute _ = liftM3 (,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))

instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	AttrType (a,b,c,d) (x,y,z,w) where
	setAttribute _ = liftM4 (,,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))
		(setAttribute (bottom :: d))

-- ~ instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w, AttrType e v) =>
	-- ~ AttrType (a,b,c,d,e) (x,y,z,w,v) where
	-- ~ setAttribute _ = liftM4 (,,,,)
		-- ~ (setAttribute (bottom :: a))
		-- ~ (setAttribute (bottom :: b))
		-- ~ (setAttribute (bottom :: c))
		-- ~ (setAttribute (bottom :: d))
		-- ~ (setAttribute (bottom :: e))

attribPartsVec ::
	( ShaderEnv m, Monad m, GLtype a, Storable a
	, GLtype a, GLtype (v a), Storable a, Storable (v a), Vector v)
	=> v a -> m (v (Expr V a))
attribPartsVec a = vecParts <$> setupAttribute1 a

vecParts :: (GLtype a, Vector v) => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill bottom $ map (\i -> arrV e i) $ map literal [0..]

literal :: (Show b, GLtype a) => b -> Expr e a
literal x = liftExpr (show x) []

arrV :: (GLtype a, Vector v) => Expr e (v a) -> Expr e Int32 -> Expr e a
arrV = liftE2 "[]"

instance AttrType (V2 Float) (V2 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V2 Int32) (V2 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V2 Bool)  (V2 (Expr V Bool)) where setAttribute = attribPartsVec


instance AttrType (V3 Float) (V3 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V3 Int32) (V3 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V3 Bool)  (V3 (Expr V Bool)) where setAttribute = attribPartsVec

instance AttrType (V4 Float) (V4 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V4 Int32) (V4 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V4 Bool)  (V4 (Expr V Bool)) where setAttribute = attribPartsVec


-- TESTBED -------------------------------------------------------------------------------

colorful :: HandShdr m => Mat V3 V3 Float -> VArray (V3 Float, V3 Float) -> m Bool
colorful = shader colorful'

colorful'
	:: Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float), V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful' r (n,v) = let
	v' = r **| v
	n' = transfer' n
	in (up 1 v', up 1 n' * 0.5 + 0.2)



transfer' :: V3 (Expr V Float) -> V3 (Expr F Float)
transfer' = undefined

transfer :: Expr V a -> Expr F a -- extend to all variants
transfer = undefined



