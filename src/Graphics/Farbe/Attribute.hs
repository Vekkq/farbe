{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Attribute where

import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Utility
import Graphics.Farbe.VertexArray
import Graphics.Farbe.State
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.BuildShader

import Foreign hiding (void)
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Reader

#define bottom undefined



type Vao = GLuint

-- | Make VAO
setAttributes :: (MonadIO m, AttrType a b, Farbe m, ShaderEnv m) => a -> m (Vao, b)
setAttributes a = do
	i <- glGenVertexArray
	glBindVertexArray i
	setByteMax (sizeOf a)
	e <- setAttribute a
	return (i, e)


setupAttribute1
	:: (Farbe m, ShaderEnv m, Monad m, GLtype a, Storable a) => a -> m (Expr V a)
setupAttribute1 a = do
	s <- getShaderId
	n <- name "a" a
	maxSize <- getByteMax
	o <- advanceBy a
	postShader $ withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c
		when (p < 2^15-1) $ do
			glVertexAttribPointer p
				(glComponents a)
				(glType a)
				(glNormalized a)
				(itoi $ maxSize)
				(intPtrToPtr $ IntPtr o)
			glEnableVertexAttribArray p
		-- ~ liftIO $ putStrLn $ "sl pos: " ++ show p ++ "\t arr pos: " ++ show o ++ "\t stride: " ++ (show $ itoi $ maxSize - sizeOf a) ++ "\t components: " ++ (show $ glComponents a)
	return $ liftExprShdr' $ do
		addHeader "attribute" a n
		return n

class Storable a => AttrType a b | a -> b, b -> a where
	setAttribute :: (Farbe m, ShaderEnv m, MonadIO m) => a -> m b


instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

-- not working that well and probably not necessary
-- ~ instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	-- ~ setAttribute a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 a
		-- ~ where
		-- ~ fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
		-- ~ fmap2 f = fmap (fmap f)


instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute _ = liftM2 (,) (setAttribute (bottom :: a)) (setAttribute (bottom :: b))

instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	setAttribute _ = liftM3 (,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))

instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	AttrType (a,b,c,d) (x,y,z,w) where
	setAttribute _ = liftM4 (,,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))
		(setAttribute (bottom :: d))

-- ~ instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w, AttrType e v) =>
	-- ~ AttrType (a,b,c,d,e) (x,y,z,w,v) where
	-- ~ setAttribute _ = liftM4 (,,,,)
		-- ~ (setAttribute (bottom :: a))
		-- ~ (setAttribute (bottom :: b))
		-- ~ (setAttribute (bottom :: c))
		-- ~ (setAttribute (bottom :: d))
		-- ~ (setAttribute (bottom :: e))

attribPartsVec ::
	( Farbe m, ShaderEnv m, Monad m, GLtype a, Storable a
	, GLtype a, GLtype (v a), Storable a, Storable (v a), Vector v)
	=> v a -> m (v (Expr V a))
attribPartsVec a = vecParts <$> setupAttribute1 a

instance AttrType (V2 Float) (V2 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V2 Int32) (V2 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V2 Bool)  (V2 (Expr V Bool)) where setAttribute = attribPartsVec


instance AttrType (V3 Float) (V3 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V3 Int32) (V3 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V3 Bool)  (V3 (Expr V Bool)) where setAttribute = attribPartsVec

instance AttrType (V4 Float) (V4 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V4 Int32) (V4 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V4 Bool)  (V4 (Expr V Bool)) where setAttribute = attribPartsVec

-- apparently, matrices as attributes are not outright supported in ES 2 .
-- would need to upload several vertices separately and combine them back in shader.
-- ~ attribPartsMat
	-- ~ :: ( Farbe m, ShaderEnv m, Monad m, GLtype a, Storable a
		 -- ~ , GLtype (v (v a)), GLtype (v a), GLtype a, Storable (v (v a)), Vector v
		 -- ~ )
	-- ~ => v (v a) -> m (v (v (Expr V a)))
-- ~ attribPartsMat a = (fmap vecParts . vecParts) <$> setupAttribute1 a

-- ~ instance AttrType (V2 (V2 Float)) (V2 (V2 (Expr V Float))) where
	-- ~ setAttribute = attribPartsMat

-- ~ instance AttrType (V3 (V3 Float)) (V3 (V3 (Expr V Float))) where
	-- ~ setAttribute = attribPartsMat

-- ~ instance AttrType (V4 (V4 Float)) (V4 (V4 (Expr V Float))) where
	-- ~ setAttribute = attribPartsMat


-- "disallowed by spec"
-- ~ instance (Storable a, GLtype a, KnownNat s) => AttrType (Arr s a) (Expr V (Arr s a)) where
	-- ~ setAttribute s a = setupAttribute1 s a

