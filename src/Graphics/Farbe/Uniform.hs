{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Uniform where

import Graphics.Farbe.Expr
import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Utility
import Graphics.Farbe.Array
import Graphics.Farbe.Texture


import Foreign hiding (void)
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import GHC.TypeNats

#define bottom undefined


data Var a = Var ExprI


class Uniform a b | a -> b, b -> a where
	uniformUpload :: (MonadIO m, HandTex m) => GLint -> a -> m ()
	uniformExpr :: Int -> a -> (String, b)

nameUniform :: GLtype a => Int -> a -> String
nameUniform i a = "u" ++ show i ++ glShortName a

exprUniform :: GLtype a => Int -> a -> (String, Expr e b)
exprUniform i a = ((nameUniform i a), Expr $ ExprI (nameUniform i a) (toTypeS a) [] RegisterUniform)

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)



-- ~ instance Uniform Float (Expr V Float) where
	-- ~ uniformUpload l = glUniform1f l
	-- ~ uniformExpr = exprUniform

-- ~ instance Uniform Int32 (Expr V Int32) where
	-- ~ uniformUpload l = glUniform1i l . itoi
	-- ~ uniformExpr = exprUniform

-- ~ instance Uniform Bool (Expr V Bool) where
	-- ~ uniformUpload l = glUniform1i l . boolToInt
	-- ~ uniformExpr = exprUniform

-- ~ instance Uniform (V2 Float) (V2 (Expr V Float)) where
	-- ~ uniformUpload l (V2 a b) = glUniform2f l a b
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V3 Float) (V3 (Expr V Float)) where
	-- ~ uniformUpload l (V3 a b c) = glUniform3f l a b c
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V4 Float) (V4 (Expr V Float)) where
	-- ~ uniformUpload l (V4 a b c d) = glUniform4f l a b c d
	-- ~ uniformExpr = fmap vecParts .: exprUniform


-- ~ instance Uniform (V2 Int32) (V2 (Expr V Int32)) where
	-- ~ uniformUpload l (V2 a b) = glUniform2i l (itoi a) (itoi b)
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V3 Int32) (V3 (Expr V Int32)) where
	-- ~ uniformUpload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V4 Int32) (V4 (Expr V Int32)) where
	-- ~ uniformUpload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)
	-- ~ uniformExpr = fmap vecParts .: exprUniform


-- ~ instance Uniform (V2 Bool) (V2 (Expr V Bool)) where
	-- ~ uniformUpload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V3 Bool) (V3 (Expr V Bool)) where
	-- ~ uniformUpload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)
	-- ~ uniformExpr = fmap vecParts .: exprUniform

-- ~ instance Uniform (V4 Bool) (V4 (Expr V Bool)) where
	-- ~ uniformUpload l (V4 a b c d) =
		-- ~ glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)
	-- ~ uniformExpr = fmap vecParts .: exprUniform


-- ~ instance Uniform (Mat V2 V2 Float) (Mat V2 V2 (Expr V Float)) where
	-- ~ uniformUpload l (V2 (V2 a b) (V2 c d)) = glUniform4f l a b c d
	-- ~ uniformExpr = fmap matParts .: exprUniform

-- ~ instance Uniform (Mat V3 V3 Float) (Mat V3 V3 (Expr V Float)) where
	-- ~ uniformUpload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p
	-- ~ uniformExpr = fmap matParts .: exprUniform

-- ~ instance Uniform (Mat V4 V4 Float) (Mat V4 V4 (Expr V Float)) where
	-- ~ uniformUpload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p
	-- ~ uniformExpr = fmap matParts .: exprUniform


-- ~ instance Uniform Texture (Expr V Texture) where
	-- ~ uniformUpload = texUpload
	-- ~ uniformExpr = exprUniform


withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray


instance GLtype Texture where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"

