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
import Graphics.Farbe.Texture


import Foreign hiding (void)
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad.Reader


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
	upload l m = withArray' (toList2 m) $ glUniformMatrix3fv l 1 GL_FALSE

instance Uniform (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ glUniformMatrix4fv l 1 GL_FALSE

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

varName :: Var a -> String
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

instance Use (Var (V2 (V2 Float))) (Mat V2 V2 (Expr F Float)) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) (Mat V3 V3 (Expr F Float)) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) (Mat V4 V4 (Expr F Float)) where use = usePartsMat
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)




class UseFun f r | f -> r where
	useFun :: f -> r


instance UseFun (a -> SResult) (a -> SResult) where useFun f a = f a

-- ~ instance (Use v e, UseFun f g) => UseFun
	-- ~ (e -> f)
	-- ~ (v -> g) where
	-- ~ useFun f v = useFun $ f (use v)

instance (Use v e) => UseFun
	(e -> a -> SResult)
	(v -> a -> SResult) where
	useFun f v a = f (use v) a

instance (Use v1 e1, Use v2 e2) => UseFun
	(e2 -> e1 -> a -> SResult)
	(v2 -> v1 -> a -> SResult) where
	useFun f v2 v1 a = f (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3) => UseFun
	(e3 -> e2 -> e1 -> a -> SResult)
	(v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v3 v2 v1 a = f (use v3) (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3, Use v4 e4) => UseFun
	(e4 -> e3 -> e2 -> e1 -> a -> SResult)
	(v4 -> v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v4 v3 v2 v1 a = f (use v4) (use v3) (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3, Use v4 e4, Use v5 e5) => UseFun
	(e5 -> e4 -> e3 -> e2 -> e1 -> a -> SResult)
	(v5 -> v4 -> v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v5 v4 v3 v2 v1 a =
		f (use v5) (use v4) (use v3) (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3, Use v4 e4, Use v5 e5, Use v6 e6) => UseFun
	(e6 -> e5 -> e4 -> e3 -> e2 -> e1 -> a -> SResult)
	(v6 -> v5 -> v4 -> v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v6 v5 v4 v3 v2 v1 a =
		f (use v6) (use v5) (use v4) (use v3) (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3, Use v4 e4, Use v5 e5, Use v6 e6, Use v7 e7) => UseFun
	(e7 -> e6 -> e5 -> e4 -> e3 -> e2 -> e1 -> a -> SResult)
	(v7 -> v6 -> v5 -> v4 -> v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v7 v6 v5 v4 v3 v2 v1 a =
		f (use v7) (use v6) (use v5) (use v4) (use v3) (use v2) (use v1) a

instance (Use v1 e1, Use v2 e2, Use v3 e3, Use v4 e4, Use v5 e5, Use v6 e6, Use v7 e7, Use v8 e8) => UseFun
	(e8 -> e7 -> e6 -> e5 -> e4 -> e3 -> e2 -> e1 -> a -> SResult)
	(v8 -> v7 -> v6 -> v5 -> v4 -> v3 -> v2 -> v1 -> a -> SResult) where
	useFun f v8 v7 v6 v5 v4 v3 v2 v1 a =
		f (use v8) (use v7) (use v6) (use v5)
		  (use v4) (use v3) (use v2) (use v1) a
