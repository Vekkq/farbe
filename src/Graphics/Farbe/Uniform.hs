{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.Uniform where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
import Graphics.Farbe.Shader
import Graphics.Farbe.Texture
import Graphics.Farbe.Utils
import Graphics.Farbe.Array

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Function
import Data.Foldable
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Data.Array.MArray as MA
import Numeric
import Foreign hiding (void)
import Foreign.C



-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats
import Data.Proxy

import System.Mem.StableName

data Var a = Var { varExpr :: ExprEnv, varMVar :: MVar a }

swapVar :: MonadIO m => Var a -> a -> m a
swapVar v = liftIO . swapMVar (varMVar v)

readVar :: MonadIO m => Var a -> m a
readVar = liftIO . readMVar . varMVar


makeVar :: forall a m . (Count m, MonadIO m, GLtype a, Upload m a) => a -> m (Var a)
makeVar a = do
	m <- liftIO $ newMVar a
	vname <- (name "u" a)
	let r = do
		addHeader "uniform" a vname
		s <- getShader
		defer $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged $ upload l
			defer $ (liftIO $ readMVar m) >>= runwc wc
		return vname
	return $ Var (ExprEnv r (toTypeS (err :: a)) []) m

setupUpload' :: (Eq a, MonadGL m, PreRender m) => (a -> GL IO ()) -> MVar a -> m ()
setupUpload' f m = do
	wc <- makeRunWhenChanged f
	preRender $ (liftIO $ readMVar m) >>= runwc wc


class Use a e r | a e -> r, r -> a e where
	use :: a -> r

instance Use (Var Float) e (Expr e Float) where use = Expr . varExpr
instance Use (Var Int32) e (Expr e Int32) where use = Expr . varExpr
instance Use (Var Bool) e (Expr e Bool) where use = Expr . varExpr

usePartsVec :: (Vector v, GLtype a) => Var (v a) -> v (Expr e a)
usePartsVec = vecParts . Expr . varExpr

instance Use (Var (V2 Float)) e (V2 (Expr e Float)) where use = usePartsVec
instance Use (Var (V2 Int32)) e (V2 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V2 Bool)) e (V2 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V3 Float)) e (V3 (Expr e Float)) where use = usePartsVec
instance Use (Var (V3 Int32)) e (V3 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V3 Bool)) e (V3 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V4 Float)) e (V4 (Expr e Float)) where use = usePartsVec
instance Use (Var (V4 Int32)) e (V4 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V4 Bool)) e (V4 (Expr e Bool)) where use = usePartsVec

usePartsMat :: (Vector v, GLtype a, GLtype (v a)) => Var (v (v a)) -> v (v (Expr e a))
usePartsMat v = vecParts <$> vecParts (Expr $ varExpr v)

instance Use (Var (V2 (V2 Float))) e (V2 (V2 (Expr e Float))) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) e (V3 (V3 (Expr e Float))) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) e (V4 (V4 (Expr e Float))) where use = usePartsMat

instance (KnownNat s, GLtype a) => Use (Var (Arr s a)) e (Expr e (Arr s a)) where
	use = Expr . varExpr



instance Use (Var (Texture f)) e (Expr e (Texture f)) where
  use = Expr . varExpr

-- add expr texture shader access functions

class (MonadIO m, GLtype a, Eq a) => Upload m a where
	upload :: GLint -> MVar a -> m ()

instance MonadIO m => Upload m Bool where upload l = glUniform1i l . boolToInt
instance MonadIO m => Upload m Int32 where upload l = glUniform1i l . itoi
instance MonadIO m => Upload m Float where	upload l = glUniform1f l
instance MonadIO m => Upload m (V2 Float) where upload l (V2 a b) = glUniform2f l a b
instance MonadIO m => Upload m (V3 Float) where upload l (V3 a b c) = glUniform3f l a b c
instance MonadIO m => Upload m (V4 Float) where upload l (V4 a b c d) = glUniform4f l a b c d

instance MonadIO m => Upload m (V2 Int32) where
	upload l (V2 a b) = glUniform2i l (itoi a) (itoi b)

instance MonadIO m => Upload m (V3 Int32) where
	upload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)

instance MonadIO m => Upload m (V4 Int32) where
	upload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)

instance MonadIO m => Upload m (V2 Bool) where
	upload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)

instance MonadIO m => Upload m (V3 Bool) where
	upload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)

instance MonadIO m => Upload m (V4 Bool) where
	upload l (V4 a b c d) =
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)


instance MonadIO m => Upload m (Mat V2 V2 Float) where
	upload l = (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance MonadIO m => Upload m (Mat V3 V3 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p

instance MonadIO m => Upload m (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p



instance (MonadIO m, HandTex m) => Upload m (Texture f) where
	upload l (Texture i u c w h) = do
		TexState u' ts <- getTex
		i' <- if (u == 0) then return 0 else liftIO $ readArray ts u -- what does this do tho?
		when (i /= i') $ do
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			swapVar m $ Texture i u' c w h
			u'' <- succU ts u'
			liftIO $ writeArray ts u'' i
			setTex $ TexState u'' ts
		where
		succU ts x = do
			let x' = succ x
			(l,h) <- liftIO $ getBounds ts
			return $ if x' >= h then l else x'
