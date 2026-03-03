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
import Graphics.Farbe.Utility
import Graphics.Farbe.ShaderCache

import Data.Char
import Data.List
import Data.Foldable
import Data.Hashable
import Foreign hiding (void)
import Foreign.C
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.IntMap as M


import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

#define bottom undefined

-- ~ import GHC.Stack
-- ~ import Debug.Trace



instance (Monad m, ShaderEnv m) => ShaderEnv (BuildShaderT m) where
	stateShader = lift . stateShader


type ShaderM = DeferT' Shdr

type ShaderDef = ShaderM (V4 (Expr V Float), V4 (Expr F Float))


compileShader :: (Farbe m, AttrType a b)
	=> (b -> ShaderDef) -> m ShExec
compileShader f = do
	(vao, sd) <- createShader $ do
		join $ addShader GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes (bottom :: a)
			((vs,fs),fm) <- runDeferT $ f e
			addExpr "gl_Position" $ exprVec vs
			return $ addShader GL_FRAGMENT_SHADER $ do
				addExpr "gl_FragColor" $ exprVec $ fs
				sequence_ fm -- fm are operations for fragment shader part
				-- splice fs here to add further outputs
				return i
	let	completeShader = do
			liftIO $ glUseProgram $ shaderId sd
			liftIO $ glBindVertexArray vao
			preRenderM sd
	return (shaderId sd, completeShader)

addShader :: (Farbe m, ShaderEnv m) => GLenum -> BuildShaderT m a -> m a
addShader t shdr = do
	sp <- getShaderId
	(a,st) <- runBuildShader shdr
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ toCStatements (bexpr st)
		++ "}"
	liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		err <- checkShaderError str i
		maybe (return ()) (putStrLn . (str++)) err
		glAttachShader sp i
		when (t == GL_FRAGMENT_SHADER) $ glLinkProgram sp
	devDebug str
	return a
	where
		checkShaderError :: String -> GLuint -> IO (Maybe String)
		checkShaderError str shdr = bracket (mallocArray $ 2^10) free $ \er ->
			bracket malloc free $ \errLength -> do
				glGetShaderInfoLog shdr (2^10) errLength er
				peekArray0 (CChar 0) er >>= \ce -> case map castCCharToChar ce of
					"" -> return Nothing
					e -> return $ Just e


isShaderCompiled :: (Farbe m, AttrType a b) => (b -> ShaderDef) -> m Bool
isShaderCompiled f = do
	msh <- lookupShader f
	case msh of
		Just (id,_) -> isShaderCompiled' id
		Nothing -> return False
	where
		isShaderCompiled' :: MonadIO m => ShaderId -> m Bool
		isShaderCompiled' id = fmap (0<) $ withPtr_ $ \p -> glGetShaderiv id GL_COMPILE_STATUS p

getExpr :: (Farbe m, AttrType a b)
	=> (b -> ShaderDef)
	-> m (V4 (Expr V Float), V4 (Expr F Float))
getExpr f = fmap (fst . fst) $ createShader $ runBuildShader $ do
	(i,e) <- setAttributes (bottom :: a)
	((vs,fs),fm) <- runDeferT $ f e
	modifyShader $ \s -> s { postShaderM = return () } -- stops it from writing GL commands
	return (vs,fs)

lookupShader :: (Farbe m, AttrType a b) => (b -> ShaderDef) -> m (Maybe ShExec)
lookupShader f = do
	e <- hash <$> getExpr f
	sc <- getShaderCache
	return $ M.lookup e sc


shader :: (Farbe m, AttrType a b)
	=> (b -> ShaderDef)
	-> [VArray a] -> m ()
shader f varrs = do
	msh <- lookupShader f
	case msh of
		Just (_,sh) -> do
			liftFarbe sh >> drawArrays varrs
		Nothing -> do
			sh <- compileShader f
			e <- hash <$> getExpr f
			modifyShaderCache $ M.insert e sh

-- ~ shader :: (MonadIO m, Farbe m, AttrType a b)
	-- ~ => (b -> ShaderDef)
	-- ~ -> [VArray a] -> m ()
-- ~ shader f varrs = do
	-- ~ mio <- lookupShader f
	-- ~ case mio of
		-- ~ Just (_, io) -> do
			-- ~ liftFarbe $ io >> drawArrays varrs
		-- ~ Nothing -> do
			-- ~ shExec <- compileShader f
			-- ~ e <- getExpr f
			-- ~ sc <- getShaderCache
			-- ~ insertdm e shExec sc >>= putShaderCache
			-- ~ return ()

-- ~ lookupShader :: (Farbe m, AttrType a b) => Int -> (b -> ShaderDef) -> m (Maybe ShExec)
-- ~ lookupShader l f = do
	-- ~ sc <- getShaderCache
	-- ~ case M.lookup l sc of
		-- ~ Just s -> return $ Just s
		-- ~ Nothing -> do
			-- ~ e <- hash <$> getExpr f
			-- ~ return $ M.lookup e sc

-- ~ toHead :: Foldable f => f a -> Maybe a
-- ~ toHead f = case toList f of
	-- ~ (x:_) -> Just x
	-- ~ _ -> Nothing

-- ~ isCompiled :: (Farbe m, AttrType a b)
	-- ~ => (b -> ShaderDef) -> m Bool
-- ~ isCompiled f = do
	-- ~ let i = traceShowId getThisLine
	-- ~ msh <- lookupShader i f
	-- ~ maybe (return False) (isShaderCompiled . fst) msh



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

instance (Monad m, ShaderEnv m) => ShaderEnv (DeferT' m) where
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
