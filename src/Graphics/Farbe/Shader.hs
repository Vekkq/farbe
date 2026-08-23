{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

-- ~ {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Graphics.Farbe.Shader where

import Graphics.Farbe.Expr
import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Uniform
-- ~ import Graphics.Farbe.State
-- ~ import Graphics.Farbe.BuildShader
-- ~ import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Utility
-- ~ import Graphics.Farbe.Expr

import Data.Char
import Data.List
import Data.Maybe
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





--- ShdrState - Saving global shaders ----------------------------------------------------

data ShdrState = ShdrState { shdrMap :: M.IntMap Dynamic }

class Monad m => HandShdr m where
	stateShdr :: (ShdrState -> (a, ShdrState)) -> m a

	-- ~ delayShdr :: m ((ShdrState -> ShdrState) -> IO ())

getShdr :: HandShdr m => m ShdrState
getShdr = stateShdr (\s -> (s, s))

setShdr :: HandShdr m => ShdrState -> m ()
setShdr s = stateShdr (\_ -> ((), s))


-- SHADER DEFINITION ---------------------------------------------------------------------

class Shader m f g | m f -> g, g -> f, g -> m where
	mkShader :: MonadIO m => f -> ShaderEnvT m g
	idShader :: f -> m Int

setUniform = undefined

instance (Attribute a b, Uniform u1 e1, AppliableF m (m Bool))
	=> Shader m (e1 -> b -> (V4 (Expr V Float), V4 (Expr F Float))) (u1 -> [VArray a] -> m Bool) where
	mkShader f = do
		s <- getsShader shaderId
		let vname = "foo"
		-- ~ addHeader
		g <- mkShader $ f (uniformExpr (bottom :: u1))
		l <- withString vname $ glGetUniformLocation s
		return $ \m -> if l > 0 then applyF g $ liftIO $ uniformUpload l m else g

	idShader = undefined


instance (Attribute a b)
	=> Shader m (b -> (V4 (Expr V Float), V4 (Expr F Float))) ([VArray a] -> m Bool) where
	mkShader f = do
		s <- getsShader shaderId
		(vaoId, expr, sp) <- setAttributes s (bottom :: a)
		let (exprV, exprF) = f expr
		addExpr Vertex "gl_Position" $ unExpr $ exprVec exprV
		addExpr Fragment "gl_FragColor" $ unExpr $ exprVec exprF
		optimizeExpressions
		handleTransfers
		collectHeaders
		compileSubShader Vertex
		compileSubShader Fragment
		liftIO sp
		return $ \vs -> do
			glUseProgram s
			glBindVertexArray vaoId
			drawArrays vs
			return True -- determine from separate id check

	idShader = undefined


isShaderCompiled :: f -> m Bool
isShaderCompiled = undefined

shaderCompileProgress :: f -> m [(String, Bool)] -- should i have this?
shaderCompileProgress = undefined

runShader :: (MonadIO m, Shader m f g, JoinF m g, HandShdr m) => f -> g
runShader = joinF . compileShader

compileShader :: (MonadIO m, Shader m f g, HandShdr m) => f -> m g
compileShader f = do
	(g, sd) <- runShaderEnvT $ do
		g <- mkShader f
		return g
	return g


optimizeExpressions :: ShaderEnv m => m ()
optimizeExpressions = return () -- undefined


handleTransfers :: ShaderEnv m => m ()
handleTransfers = do
	exps <- getsShader exprs
	exps' <- sequence $ for exps $ \(t,(s,e)) -> fmap (\e -> (t,(s,e))) $ mapExpr f e
	modifyShader $ \s -> s { exprs = exps' }
	where
	f :: ShaderEnv m => ExprI -> m ExprI
	f (ExprI "transferFrag" t [p] r) = do
		c <- stateShader $ \case s | c <- succ $ counter s -> (c, s { counter = c })
		let name = "t" ++ show c ++ slNameFromTypeS t
		addExpr Vertex name p
		return $ ExprI name t [] RegisterVarying
	f e = return e

-- ~ mapExpr :: Monad m => (ExprI -> m ExprI) -> ExprI -> m ExprI


collectHeaders :: ShaderEnv m => m ()
collectHeaders = do
	exps <- getsShader exprs
	forM_ exps $ \(e, (s, exp)) -> sequence_ $ crawl (addHeader e) exp
-- vertexes, unicodes, transfers


addHeader :: (ShaderEnv m) => ShaderType -> ExprI -> m ()
addHeader e (ExprI n a ps r) = do
	let i = case r of
		RegisterUniform -> "uniform"
		RegisterVertex -> "attribute"
		RegisterVarying -> "varying"
		RegisterNone -> ""
	let str = unwords [i, slNameWithPrecTypeS a, n, ";"]
	hs <- getsShader headers
	let b = not (null i) && not (S.member (e,str) hs)
	when b $ modifyShader $ \s -> s { headers = S.insert (e,str) $ headers s }


compileSubShader :: (MonadIO m, ShaderEnv m) => ShaderType -> m ()
compileSubShader t = do
	st <- getShader
	let str
		=  "#version 100\n"
		++ (unlines $ pickForShader $ toList $ headers st)
		++ "\n\nvoid main(){\n"
		++ (toCStatements $ pickForShader $ exprs st)
		++ "}"
	sp <- getsShader shaderId
	liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader $ shaderTypeGLEnum t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		err <- checkShaderError i
		maybe (return ()) (putStrLn . (str++)) err
		glAttachShader sp i
		when (t == Fragment) $ glLinkProgram sp
	where
		checkShaderError :: GLuint -> IO (Maybe String)
		checkShaderError i = bracket (mallocArray $ 2^10) free $ \er ->
			bracket malloc free $ \errLength -> do
				glGetShaderInfoLog i (2^10) errLength er
				peekArray0 (CChar 0) er >>= \ce -> case map castCCharToChar ce of
					"" -> return Nothing
					e -> return $ Just e

		pickForShader :: [(ShaderType, a)] -> [a]
		pickForShader = map snd . filter ((t==). fst)

		shaderTypeGLEnum :: ShaderType -> GLenum
		shaderTypeGLEnum Vertex = GL_VERTEX_SHADER
		shaderTypeGLEnum Fragment = GL_FRAGMENT_SHADER

toCStatements :: [(String, ExprI)] -> String
toCStatements xs = unlines $ reverse $ for xs $ \(s,e) -> s ++ " = " ++ toCExpr e ++";"


toCExpr :: ExprI -> String
toCExpr e = case e of
	ExprI s _ [] _ -> s
	ExprI "[]" _ (p1:p2:[]) _ -> toCExpr p1 ++ "[" ++ toCExpr p2 ++ "]"
	ExprI s _ (p1:p2:[]) _ | isOp s -> par $ toCExpr p1 ++ s ++ toCExpr p2
	ExprI "if" _ (p1:p2:p3:[]) _ -> par $ toCExpr p1 ++ "?" ++ toCExpr p2 ++ ":" ++ toCExpr p3
	ExprI s _ as _ -> (s++) $ par $ intercalate ", " $ map toCExpr as
	where
		isOp :: String -> Bool
		isOp (x:_) = not $ isAlpha x
		isOp [] = False

		par :: String -> String
		par s = "(" ++ s ++ ")"


class JoinF m f where
	joinF :: m f -> f

instance {-# INCOHERENT #-} (JoinF m f, Monad m) => JoinF m (a -> f) where
	joinF mf = \a -> joinF $ fmap ($ a) mf

instance Monad m => JoinF m (m a) where
	joinF = join


class LiftF nm f g | f nm -> g, g nm -> f where
	liftF :: nm -> f -> g

instance {-# INCOHERENT #-} LiftF nm f g => LiftF nm (a -> f) (a -> g) where
	liftF nm f = \a -> liftF nm (f a)

instance LiftF (n -> m) n m where
	liftF nm n = nm n


class AppliableF m f | f -> m where
	applyF :: f -> m a -> f

instance {-# INCOHERENT #-} AppliableF m b => AppliableF m (a -> b) where
	applyF f m = \p -> applyF (f p) m

instance Applicative m => AppliableF m (m a) where
	applyF f m = m *> f




(.:) = (.).(.)


------------------------------------------------------------------------------------------

-- ~ type ShaderId = GLuint

data ShaderType = Vertex | Fragment deriving (Eq, Ord, Read, Show, Enum)

data ShaderData = ShaderData
	{ counter :: Int
	, shaderId :: ShaderId
	, headers :: S.Set (ShaderType, Header)
	, exprs :: [(ShaderType, (String, ExprI))]
	-- ~ , preRender :: IO ()
	}

type Header = String

emptyShaderData :: MonadIO m => m ShaderData
emptyShaderData = do
	i<- glCreateProgram
	return $ ShaderData
		{ counter = 0
		, shaderId = i
		, headers = S.empty
		, exprs = []
		-- ~ , preRender = return ()
		}

newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT ShaderData m a }
	deriving
		(Functor, Applicative, Monad, MonadIO)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Monad m) => ShaderEnv m where
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

runShaderEnvT :: MonadIO m => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = emptyShaderData >>= runStateT ms


addExpr :: ShaderEnv m => ShaderType -> String -> ExprI -> m ()
addExpr e n expri = do
	modifyShader $ \s -> s { exprs = (e,(n,expri)) : exprs s }




