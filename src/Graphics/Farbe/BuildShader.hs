{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.BuildShader where


import Graphics.Farbe.GL
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.State
import Graphics.Farbe.Vec

import qualified Data.Set as S
import Data.Foldable
import Foreign hiding (void)
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict



#define bottom undefined

-- | User-facing type expression.
data Expr e a = Expr { unExpr :: ExprI } deriving Functor

-- | Internal expression description.
data ExprI = ExprI { fnName :: Shdr String, rtype :: TypeS, fnAst :: [ExprI] }

data ExprS = ExprS String TypeS [ExprS] deriving Show

-- | A Shader-building environment.
type Shdr = BuildShaderT (ShaderEnvT (FarbeT IO))

-- ~ data ExprI = ExprI { exprName :: String, exprSetup :: BuildShaderT (ShaderEnvT IO)
	-- ~ , exprType :: TypeS, exprAst :: [ExprI] }
-- TODO future rewrite form

runExprI :: ExprI -> Shdr ExprS
runExprI (ExprI m r ps) = do
	s <- m
	ps' <- mapM runExprI ps
	return $ ExprS s r ps'

liftExpr :: (GLtype a) => String -> [ExprI] -> Expr e a
liftExpr s p = liftExprShdr (return s) p

liftExpr' :: (GLtype a) => String -> Expr e a
liftExpr' s = liftExpr s []

liftExprShdr :: forall e a . (GLtype a) => Shdr String -> [ExprI] -> Expr e a
liftExprShdr s p = Expr $ ExprI s (toTypeS (bottom :: a)) p

liftExprShdr' :: (GLtype a) => Shdr String -> Expr e a
liftExprShdr' s = liftExprShdr s []


expr :: (Show b, GLtype a) => b -> Expr e a
expr x = liftExpr (show x) []


-- overload it for multiple parameters

liftE0 ::(GLtype a) => String -> Expr e a
liftE0 s = liftExpr s []

liftE1 :: (GLtype a2) => String -> Expr e a1 -> Expr e a2
liftE1 s (Expr a) = liftExpr s [a]

liftE2 :: (GLtype a3) => String -> Expr e a1 -> Expr e a2 -> Expr e a3
liftE2 s (Expr a) (Expr b) = liftExpr s [a,b]

liftE3 :: (GLtype a4) => String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
liftE3 s (Expr a) (Expr b) (Expr c) = liftExpr s [a,b,c]


vecParts :: (GLtype a, Vector v) => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill bottom $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall e a v . (GLtype a, Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = liftExpr (slName (bottom :: v a)) $ map unExpr $ toList v

exprMat :: forall a e v .(GLtype a, Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr e a)) -> Expr e (v (v a))
exprMat v = liftExpr (slName (bottom :: v a)) $ map unExpr $ concatMap toList $ toList v

arrV :: (GLtype a, Vector v) => Expr e (v a) -> Expr e Int32 -> Expr e a
arrV = liftE2 "[]"



-- Shader building monad -----------------------------------------------------------------

data BuildShaderState = BuildShaderState
	{ header :: S.Set String
	, bexpr :: [(String, ExprS)]
	}

emptyShaderState :: BuildShaderState
emptyShaderState = BuildShaderState S.empty []

newtype BuildShaderT m a = BuildShaderT { unBuildShaderT :: StateT BuildShaderState m a }
	deriving
		(Functor, Applicative, Monad, MonadIO, MonadTrans, Farbe)

instance MonadState s m => MonadState s (BuildShaderT m) where
	state = lift . state


runBuildShader :: BuildShaderT m a -> m (a, BuildShaderState)
runBuildShader  b = runStateT (unBuildShaderT b) emptyShaderState


class Monad m => BuildShader m where
	buildShaderState :: (BuildShaderState -> (a, BuildShaderState)) -> m a

	buildShaderStateGet :: m BuildShaderState
	buildShaderStateGet = buildShaderState $ \s -> (s,s)
	buildShaderStatePut :: BuildShaderState -> m ()
	buildShaderStatePut a = buildShaderState $ \_ -> ((),a)

instance Monad m => BuildShader (BuildShaderT m) where
	buildShaderState = BuildShaderT . state

-- ~ SIMPLEFUNCTION_CLASSINSTANCES(buildShaderState,BuildShader,.)

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m Bool
addHeader i a n = do
	let str = unwords [i, slNameWithPrec a, n, ";"]
	s <- buildShaderStateGet
	let b = not $ S.member str (header s)
	when b $ buildShaderStatePut $ s { header = S.insert str $ header s }
	return b


addExpr :: String -> Expr e a -> Shdr ()
addExpr n (Expr a) = do
	e <- runExprI a
	buildShaderState $ \s -> ((), s { bexpr = (n,e) : bexpr s })

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

class ShaderType a
instance ShaderType V
instance ShaderType F


instance (Monad m, ShaderEnv m) => ShaderEnv (BuildShaderT m) where
	stateShader = lift . stateShader

