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
import Graphics.Farbe.Texture
-- ~ import Graphics.Farbe.Farbe
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

data ShdrState = ShdrState
	{ shdrMap :: M.IntMap Dynamic
	, shdrCompState :: M.IntMap [String] -- list of unfinished things, were null means done
	}

initShdrState = ShdrState M.empty M.empty

class Monad m => HandShdr m where
	stateShdr :: (ShdrState -> (a, ShdrState)) -> m a

	-- ~ delayShdr :: m ((ShdrState -> ShdrState) -> IO ())

getShdr :: HandShdr m => m ShdrState
getShdr = stateShdr (\s -> (s, s))

setShdr :: HandShdr m => ShdrState -> m ()
setShdr s = stateShdr (\_ -> ((), s))

stateShdrMap :: HandShdr m => (M.IntMap Dynamic -> (a, M.IntMap Dynamic)) -> m a
stateShdrMap f = stateShdr $ \s -> let (a,sm) = f $ shdrMap s in (a, s { shdrMap = sm })

getShdrMap :: HandShdr m => m (M.IntMap Dynamic)
getShdrMap = stateShdrMap $ \s -> (s,s)

modifyShdrMap :: HandShdr m => (M.IntMap Dynamic -> M.IntMap Dynamic) -> m (M.IntMap Dynamic)
modifyShdrMap f = stateShdrMap $ \s -> (f s, f s)

-- SHADER DEFINITION ---------------------------------------------------------------------

class Shader m f g | m f -> g, g -> f, g -> m where
	mkShader :: MonadIO m => f -> ShaderEnvT m g
	idShader :: MonadIO m => f -> m Int

setUniform :: (MonadIO m, Uniform u1 e1, HandTex m, AppliableF m g, Shader m f g) => Int -> (e1 -> f) -> ShaderEnvT m (u1 -> g)
setUniform i f = do
		s <- getsShader shaderId
		let vname = "foo"
		g <- mkShader $ f $ uniformExpr i bottom
		l <- withString vname $ glGetUniformLocation s
		return $ \m -> if l > 0 then applyF g $ uniformUpload l m else g


instance (Attribute a b, Uniform u1 e1, HandTex m, AppliableF m (m Bool))
	=> Shader m
		(e1 -> b -> (V4 (Expr V Float), V4 (Expr F Float)))
		(u1 -> [VArray a] -> m Bool) where
	mkShader = setUniform 1
	idShader f = idShader $ f $ uniformExpr 1 bottom

instance (Attribute a b, Uniform u1 e1, Uniform u2 e2, HandTex m, AppliableF m (m Bool))
	=> Shader m
		(e2 -> e1 -> b -> (V4 (Expr V Float), V4 (Expr F Float)))
		(u2 -> u1 -> [VArray a] -> m Bool) where
	mkShader = setUniform 2
	idShader f = idShader $ f $ uniformExpr 2 bottom

instance (Attribute a b, Uniform u1 e1, Uniform u2 e2, Uniform u3 e3
	, HandTex m, AppliableF m (m Bool))
	=> Shader m
		(e3 -> e2 -> e1 -> b -> (V4 (Expr V Float), V4 (Expr F Float)))
		(u3 -> u2 -> u1 -> [VArray a] -> m Bool) where
	mkShader = setUniform 3
	idShader f = idShader $ f $ uniformExpr 3 bottom

instance (Attribute a b, Uniform u1 e1, Uniform u2 e2, Uniform u3 e3, Uniform u4 e4
	, HandTex m, AppliableF m (m Bool))
	=> Shader m
		(e4 -> e3 -> e2 -> e1 -> b -> (V4 (Expr V Float), V4 (Expr F Float)))
		(u4 -> u3 -> u2 -> u1 -> [VArray a] -> m Bool) where
	mkShader = setUniform 4
	idShader f = idShader $ f $ uniformExpr 4 bottom


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

	idShader f = do
		(_, expr, _) <- setAttributes 0 (bottom :: a)
		return $ hash $ f expr



isShaderCompiled :: f -> m Bool
isShaderCompiled = undefined

shaderCompileProgress :: f -> m [(String, Bool)] -- should i have this?
shaderCompileProgress = undefined

runShader' :: (MonadIO m, Shader m f g, HandShdr m, Typeable g) => f -> m g
runShader' f = do
	sm <- getShdrMap
	fid <- idShader f
	maybe (compileShader f) return $ join $ fmap fromDynamic $ M.lookup fid sm

runShader :: (MonadIO m, Shader m f g, JoinF m g, HandShdr m, Typeable g) => f -> g
runShader = joinF . runShader'


compileShader :: (MonadIO m, Shader m f g, HandShdr m, Typeable g) => f -> m g
compileShader f = do
	g <- evalShaderEnvT $ mkShader f
	-- save shader inside state
	fid <- idShader f
	modifyShdrMap $ M.insert fid (toDyn g)
	return g

saveShader :: (Shader m f g, HandShdr m) => f -> g -> m ()
saveShader f g = undefined


optimizeExpressions :: ShaderEnv m => m ()
optimizeExpressions = return () -- undefined


handleTransfers :: ShaderEnv m => m ()
handleTransfers = do
	-- TODO run mapExpr in stateT instead
	-- take all exprs
	exps <- stateShader $ \s -> (exprs s, s { exprs = [] })
	-- cut all exprs at transferfn and add the exprs after transferfn
	exps' <- sequence $ for exps $ \(t,(s,e)) -> fmap (\e -> (t,(s,e))) $ mapExpr f e
	-- add all back up
	newexprs <- stateShader $ \s -> (exprs s, s { exprs = exps' ++ exprs s })
	forM_ newexprs $ \(t,(s,e)) -> addVaryingOutputHeader s t e

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
	forM_ exps $ \(e, (s, exp)) -> sequence_ $ crawl (addInputHeader e) exp
-- vertexes, unicodes, transfers


addInputHeader :: ShaderEnv m => ShaderType -> ExprI -> m ()
addInputHeader e (ExprI n a ps r) = do
	let i = case r of
		RegisterUniform -> "uniform"
		RegisterVertex -> "attribute"
		RegisterVarying -> "varying"
		RegisterNone -> ""
	let str = unwords [i, slNameWithPrecTypeS a, n, ";"]
	hs <- getsShader headers
	let b = not (null i) && not (S.member (e,str) hs)
	when b $ modifyShader $ \s -> s { headers = S.insert (e,str) $ headers s }

addVaryingOutputHeader :: ShaderEnv m => String -> ShaderType -> ExprI -> m ()
addVaryingOutputHeader n e (ExprI _ a _ _) = let
		str = unwords ["varying", slNameWithPrecTypeS a, n, ";"]
	in modifyShader $ \s -> s { headers = S.insert (e,str) $ headers s }


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
	liftIO $ putStrLn str
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

evalShaderEnvT :: MonadIO m => ShaderEnvT m a -> m a
evalShaderEnvT = fmap fst . runShaderEnvT

addExpr :: ShaderEnv m => ShaderType -> String -> ExprI -> m ()
addExpr e n expri = do
	modifyShader $ \s -> s { exprs = (e,(n,expri)) : exprs s }




