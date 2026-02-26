{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module Graphics.Farbe.Shader where

import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
import Graphics.Farbe.State
import Graphics.Farbe.BuildShader
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Name
import Graphics.Farbe.ShaderCache

import Data.Char
import Data.List
import Data.Foldable
import Foreign hiding (void)
import Foreign.C
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

#define bottom undefined



instance (Monad m, ShaderEnv n m) => ShaderEnv n (BuildShaderT m) where
	stateShader = lift . stateShader


type ShaderM = DeferT' Shdr

compile :: (MonadIO m, Farbe m, AttrType a b)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- createShader sp $
		join $ addShader GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes (bottom :: a)
			((vs,fs),fm) <- runDeferT $ f e
			addExpr "gl_Position" $ exprVec vs
			return $ addShader GL_FRAGMENT_SHADER $ do
				addExpr "gl_FragColor" $ exprVec $ fs
			-- splice fs here to add further outputs
				sequence_ fm
				return i
	return $ \varrs -> do
		glUseProgram sp
		glBindVertexArray vao
		exec


		drawArrays varrs

	-- ~ return $ \varrs -> do
		-- ~ glUseProgram sp       -- lines to back up
		-- ~ glBindVertexArray vao --
		-- ~ exec                  --
		-- ~ drawArrays varrs


-- ~ foo :: (MonadIO m, MonadIO n, HandTex n, HandTex m, AttrType a b)
	-- ~ => (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-- ~ -> m (Maybe ([VArray a] -> n ()))
-- ~ foo = undefined

addShader :: (MonadIO m, Farbe m, ShaderEnv n m) => GLenum -> BuildShaderT m a -> m a
addShader t shdr = do
	sp <- getShaderId
	(a,st) <- runBuildShader shdr
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ toCStatements (bexpr st)
		++ "}"
	err <- liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		err <- checkShaderError str i
		glAttachShader sp i
		when (t == GL_FRAGMENT_SHADER) $ glLinkProgram sp
		return err
	maybe (return ()) (liftIO . putStrLn . (str++)) err
	devDebug str
	return a

checkShaderError :: String -> GLuint -> IO (Maybe String)
checkShaderError str shdr = bracket (mallocArray $ 2^10) free $ \er ->
	bracket malloc free $ \errLength -> do
		glGetShaderInfoLog shdr (2^10) errLength er
		peekArray0 (CChar 0) er >>= \ce -> case map castCCharToChar ce of
			"" -> do
				return Nothing
			e -> do
				return $ Just e


toCStatements :: [(String, ExprS)] -> String
toCStatements xs = unlines $ reverse $ for xs $ \(s,e) -> s ++ " = " ++ toCExpr e ++";"


toCExpr :: ExprS -> String
toCExpr e = case e of
	ExprS s _ [] -> s
	ExprS "[]" _ (p1:p2:[]) -> toCExpr p1 ++ "[" ++ toCExpr p2 ++ "]"
	ExprS s _ (p1:p2:[]) | isOp s -> par $ toCExpr p1 ++ s ++ toCExpr p2
	ExprS "if" _ (p1:p2:p3:[]) -> par $ toCExpr p1 ++ "?" ++ toCExpr p2 ++ ":" ++ toCExpr p3
	ExprS s _ as -> (s++) $ par $ intercalate ", " $ map toCExpr as
	where
		isOp :: String -> Bool
		isOp (x:_) = not $ isAlpha x
		isOp [] = False

		par :: String -> String
		par s = "(" ++ s ++ ")"


-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> ShaderM b

instance (GLtype a) => Transfer (Expr V a) (Expr F a) where
	transfer = transfer1

instance (GLtype a, GLtype (V2 a)) => Transfer (V2 (Expr V a)) (V2 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V3 a)) => Transfer (V3 (Expr V a)) (V3 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V4 a)) => Transfer (V4 (Expr V a)) (V4 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V2 a), GLtype (V2 (V2 a)))
	=> Transfer (V2 (V2 (Expr V a))) (V2 (V2 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat

instance (GLtype a, GLtype (V3 a), GLtype (V3 (V3 a))) =>
	Transfer (V3 (V3 (Expr V a))) (V3 (V3 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat

instance (GLtype a, GLtype (V4 a), GLtype (V4 (V4 a))) =>
	Transfer (V4 (V4 (Expr V a))) (V4 (V4 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat


transfer1 :: forall a . GLtype a => Expr V a -> DeferT' Shdr (Expr F a)
transfer1 e = do
	let a = bottom :: a
	n <- name "t" a
	lift $ addExpr n e
	addHeader "varying" a n
	defer $ void $ addHeader "in" a n
	return $ liftExprShdr' $ return n



-- DeferT --------------------------------------------------------------------------------
-- | DeferT, simple monad to defer monadic operations.

newtype DeferT n m a = DeferT { unDefer :: StateT (Seq.Seq n) m a }
	deriving (Functor, Applicative, Monad, MonadIO, Farbe)

type DeferT' m = DeferT (m ()) m

instance MonadTrans (DeferT n) where
	lift = DeferT . lift

instance (Monad m, BuildShader m) => BuildShader (DeferT' m) where
	buildShaderState = lift . buildShaderState

instance (Monad m, ShaderEnv n m) => ShaderEnv n (DeferT' m) where
	stateShader = lift . stateShader


runDeferT :: (Monad m) => DeferT n m a -> m (a, [n])
runDeferT m = do
	(a,w) <- runStateT (unDefer m) Seq.empty
	return (a, toList w)

runDeferT' :: Monad m => DeferT' m a -> m a
runDeferT' m = do
	(a,e) <- runDeferT m
	sequence_ e
	return a

runDeferT'' :: Monad m => DeferT n m a -> m (a, [n])
runDeferT'' m = do
	(a,w) <- runStateT (unDefer m) Seq.empty
	return (a, toList w)

class Monad m => Defer n m | m -> n where
	defer :: n -> m ()

instance (Monad m) => Defer n (DeferT n m) where
	defer = DeferT . (\a -> modify (|>a))
