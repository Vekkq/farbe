{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
-- ~ {-# LANGUAGE AllowAmbiguousTypes #-}

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


data Var a = Var { varExpr :: ExprI }


class (GLtype a, Eq a) => Uniform a where
	upload :: (MonadIO m, HandTex m) => GLint -> a -> m ()

instance Uniform Bool where upload l = glUniform1i l . boolToInt
instance Uniform Int32 where upload l = glUniform1i l . itoi
instance Uniform Float where	upload l = glUniform1f l
instance Uniform (V2 Float) where upload l (V2 a b) = glUniform2f l a b
instance Uniform (V3 Float) where upload l (V3 a b c) = glUniform3f l a b c
instance Uniform (V4 Float) where upload l (V4 a b c d) = glUniform4f l a b c d

instance Uniform (V2 Int32) where
	upload l (V2 a b) = glUniform2i l (itoi a) (itoi b)

instance Uniform (V3 Int32) where
	upload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)

instance Uniform (V4 Int32) where
	upload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)

instance Uniform (V2 Bool) where
	upload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)

instance Uniform (V3 Bool) where
	upload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)

instance Uniform (V4 Bool) where
	upload l (V4 a b c d) =
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)


instance Uniform (Mat V2 V2 Float) where
	upload l = (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance Uniform (Mat V3 V3 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p

instance Uniform (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p

instance Uniform Texture where
	upload = texUpload

withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray

instance GLtype Texture where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"


nameUniform :: GLtype a => Int -> a -> String
nameUniform i a = "u" ++ show i ++ glShortName a

uniformVar :: GLtype a => Int -> a -> Var a
uniformVar i a = Var $ ExprI (nameUniform i a) (toTypeS a) [] RegisterUniform

varName (Var e) = fnName e



class Use a r | r -> a where
	use :: a -> r

usePartsMat :: (Vector v, GLtype a, GLtype (v a)) => Var (v (v a)) -> v (v (Expr e a))
usePartsMat v = vecParts <$> vecParts (Expr $ varExpr v)

usePartsVec :: (Vector v, GLtype a) => Var (v a) -> v (Expr e a)
usePartsVec = vecParts . Expr . varExpr

instance Use (Var Float) (Expr V Float) where use = Expr . varExpr
instance Use (Var Int32) (Expr V Int32) where use = Expr . varExpr
instance Use (Var Bool) (Expr V Bool) where use = Expr . varExpr
instance Use (Var Texture) (Expr V Texture) where use = Expr . varExpr


instance Use (Var (V2 Float)) (V2 (Expr V Float)) where use = usePartsVec
instance Use (Var (V2 Int32)) (V2 (Expr V Int32)) where use = usePartsVec
instance Use (Var (V2 Bool)) (V2 (Expr V Bool)) where use = usePartsVec
instance Use (Var (V3 Float)) (V3 (Expr V Float)) where use = usePartsVec
instance Use (Var (V3 Int32)) (V3 (Expr V Int32)) where use = usePartsVec
instance Use (Var (V3 Bool)) (V3 (Expr V Bool)) where use = usePartsVec
instance Use (Var (V4 Float)) (V4 (Expr V Float)) where use = usePartsVec
instance Use (Var (V4 Int32)) (V4 (Expr V Int32)) where use = usePartsVec
instance Use (Var (V4 Bool)) (V4 (Expr V Bool)) where use = usePartsVec


instance Use (Var (V2 (V2 Float))) (V2 (V2 (Expr V Float))) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) (V3 (V3 (Expr V Float))) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) (V4 (V4 (Expr V Float))) where use = usePartsMat


instance Use (Var Float) (Expr F Float) where use = Expr . varExpr
instance Use (Var Int32) (Expr F Int32) where use = Expr . varExpr
instance Use (Var Bool) (Expr F Bool) where use = Expr . varExpr
instance Use (Var Texture) (Expr F Texture) where use = Expr . varExpr


instance Use (Var (V2 Float)) (V2 (Expr F Float)) where use = usePartsVec
instance Use (Var (V2 Int32)) (V2 (Expr F Int32)) where use = usePartsVec
instance Use (Var (V2 Bool)) (V2 (Expr F Bool)) where use = usePartsVec
instance Use (Var (V3 Float)) (V3 (Expr F Float)) where use = usePartsVec
instance Use (Var (V3 Int32)) (V3 (Expr F Int32)) where use = usePartsVec
instance Use (Var (V3 Bool)) (V3 (Expr F Bool)) where use = usePartsVec
instance Use (Var (V4 Float)) (V4 (Expr F Float)) where use = usePartsVec
instance Use (Var (V4 Int32)) (V4 (Expr F Int32)) where use = usePartsVec
instance Use (Var (V4 Bool)) (V4 (Expr F Bool)) where use = usePartsVec

instance Use (Var (V2 (V2 Float))) (V2 (V2 (Expr F Float))) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) (V3 (V3 (Expr F Float))) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) (V4 (V4 (Expr F Float))) where use = usePartsMat
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)




class UseFun f r where
	useFun :: f -> r


-- ~ instance UseFun SResult SResult where
	-- ~ useFun = id

instance (Use v e) => UseFun
	(e -> SResult)
	(v -> SResult) where
	useFun f v = f (use v)

instance (Use v1 e1, Use v2 e2) => UseFun
	(e2 -> e1 -> SResult)
	(v2 -> v1 -> SResult) where
	useFun f v2 v1 = f (use v2) (use v1)


-- ~ foo :: (Expr V Float -> SResult) -> Var Float -> SResult
-- ~ foo = useFun

-- ~ instance (Use a r, Use a2 r2) => UseFun (a2 -> a -> SResult) (r2 -> r -> SResult) where
	-- ~ useFun f a b = f (use a) (use b)

