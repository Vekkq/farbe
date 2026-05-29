
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module Graphics.Farbe.Uniform where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
import Graphics.Farbe.Utility
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Array
import Graphics.Farbe.Texture
import Graphics.Farbe.State
-- ~ import Graphics.Farbe.Shader
import Graphics.Farbe.BuildShader
import Graphics.Farbe.Name
import Graphics.Farbe.ShaderEnv


import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import System.Mem.StableName
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))


import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats


#define bottom undefined

-- Uniform variables ---------------------------------------------------------------------

data Var a = Var { varExpr :: ExprI, varMVar :: MVar a }

swapVar :: MonadIO m => Var a -> a -> m a
swapVar v = liftIO . swapMVar (varMVar v)

readVar :: MonadIO m => Var a -> m a
readVar = liftIO . readMVar . varMVar


makeVar :: forall a m . (Farbe m, MonadIO m, GLtype a, Upload a) => a -> m (Var a)
makeVar a = do
	m <- liftIO $ newMVar a
	vname <- (name "u" a)
	let r = do
		b <- addHeader "uniform" a vname
		s <- getShaderId
		when b $ postShader $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged $ upload l
			-- RunWhenChanged will probably bork for textures,
			-- since they need to be always checked for assigned tex unit
			-- it will bork when the max unit limit is exceeded,
			-- which will overwrite existing units
			preRender $ do
				(liftIO $ readMVar m) >>= runwc wc
				return True
		return vname
	return $ Var (ExprI r (toTypeS (bottom :: a)) []) m


class (GLtype a, Eq a) => Upload a where
	upload :: (MonadIO m, HandTex m) => GLint -> a -> m ()
	-- TODO: makeUploadFn :: GLint -> a -> m (a -> m ())
	-- move RunWhenChanged into instances
	-- for the purpose of separating texture upload routine

instance Upload Bool where upload l = glUniform1i l . boolToInt
instance Upload Int32 where upload l = glUniform1i l . itoi
instance Upload Float where	upload l = glUniform1f l
instance Upload (V2 Float) where upload l (V2 a b) = glUniform2f l a b
instance Upload (V3 Float) where upload l (V3 a b c) = glUniform3f l a b c
instance Upload (V4 Float) where upload l (V4 a b c d) = glUniform4f l a b c d

instance Upload (V2 Int32) where
	upload l (V2 a b) = glUniform2i l (itoi a) (itoi b)

instance Upload (V3 Int32) where
	upload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)

instance Upload (V4 Int32) where
	upload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)

instance Upload (V2 Bool) where
	upload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)

instance Upload (V3 Bool) where
	upload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)

instance Upload (V4 Bool) where
	upload l (V4 a b c d) =
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)


instance Upload (Mat V2 V2 Float) where
	upload l = (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance Upload (Mat V3 V3 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p

instance Upload (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p

instance Upload Texture where
	upload = texUpload


instance GLtype Texture where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"


withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)


makeVarT :: forall m . (Farbe m, MonadIO m) => Texture -> m (Var Texture)
makeVarT tex = do
	m <- liftIO $ newMVar tex
	vname <- (name "u" tex)
	let r = do
		b <- addHeader "uniform" tex vname
		s <- getShaderId
		when b $ postShader $ do
			l <- withString vname $ glGetUniformLocation s
			preRender $ do
				t <- liftIO $ readMVar m
				b1 <- liftIO $ isEmptyMVar $ tbase t
				when b1 $ texUpload l t
				return $ not b1 -- TODO check if this correct
		return vname
	return $ Var (ExprI r (toTypeS tex) []) m

-- makeVars ------------------------------------------------------------------------------

makeVarF :: (Farbe m, MonadIO m) => Float -> m (Var Float)
makeVarI :: (Farbe m, MonadIO m) => Int32 -> m (Var Int32)
makeVarB :: (Farbe m, MonadIO m) => Bool -> m (Var Bool)
makeVarV2F :: (Farbe m, MonadIO m) => V2 Float -> m (Var (V2 Float))
makeVarV2I :: (Farbe m, MonadIO m) => V2 Int32 -> m (Var (V2 Int32))
makeVarV2B :: (Farbe m, MonadIO m) => V2 Bool -> m (Var (V2 Bool))
makeVarV3F :: (Farbe m, MonadIO m) => V3 Float -> m (Var (V3 Float))
makeVarV3I :: (Farbe m, MonadIO m) => V3 Int32 -> m (Var (V3 Int32))
makeVarV3B :: (Farbe m, MonadIO m) => V3 Bool -> m (Var (V3 Bool))
makeVarV4F :: (Farbe m, MonadIO m) => V4 Float -> m (Var (V4 Float))
makeVarV4I :: (Farbe m, MonadIO m) => V4 Int32 -> m (Var (V4 Int32))
makeVarV4B :: (Farbe m, MonadIO m) => V4 Bool -> m (Var (V4 Bool))
makeVarM2 :: (Farbe m, MonadIO m) => (V2 (V2 Float)) -> m (Var (V2 (V2 Float)))
makeVarM3 :: (Farbe m, MonadIO m) => (V3 (V3 Float)) -> m (Var (V3 (V3 Float)))
makeVarM4 :: (Farbe m, MonadIO m) => (V4 (V4 Float)) -> m (Var (V4 (V4 Float)))
-- ~ makeVarT :: (Farbe m, MonadIO m) => Texture -> m (Var Texture)


makeVarF   = makeVar
makeVarI   = makeVar
makeVarB   = makeVar
makeVarV2F = makeVar
makeVarV2I = makeVar
makeVarV2B = makeVar
makeVarV3F = makeVar
makeVarV3I = makeVar
makeVarV3B = makeVar
makeVarV4F = makeVar
makeVarV4I = makeVar
makeVarV4B = makeVar
makeVarM2  = makeVar
makeVarM3  = makeVar
makeVarM4  = makeVar
-- ~ makeVarT   = makeVar

-- add expr texture shader access functions

makeVarEmpty :: (Farbe m, UploadDefault a) => m (Var a)
makeVarEmpty = makeVar upDefault

class Upload a => UploadDefault a where upDefault :: a

instance UploadDefault Bool where upDefault = False
instance UploadDefault Int32 where upDefault = 0
instance UploadDefault Float where upDefault = 0
instance UploadDefault (V2 Float) where upDefault = 0
instance UploadDefault (V3 Float) where upDefault = 0
instance UploadDefault (V4 Float) where upDefault = 0

instance UploadDefault (V2 Int32) where upDefault = 0
instance UploadDefault (V3 Int32) where upDefault = 0
instance UploadDefault (V4 Int32) where upDefault = 0
instance UploadDefault (V2 Bool) where upDefault = pure False
instance UploadDefault (V3 Bool) where upDefault = pure False
instance UploadDefault (V4 Bool) where upDefault = pure False
instance UploadDefault (Mat V2 V2 Float) where upDefault = 0
instance UploadDefault (Mat V3 V3 Float) where upDefault = 0
instance UploadDefault (Mat V4 V4 Float) where upDefault = 0

-- Access uniform variables --------------------------------------------------------------

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



instance Use (Var Texture) e (Expr e Texture) where
  use = Expr . varExpr

