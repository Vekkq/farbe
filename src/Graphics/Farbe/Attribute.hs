{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Attribute where

import Graphics.Farbe.Expr
import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Utility
import Graphics.GL.Ext.OES.VertexArrayObject

import Foreign hiding (void)
import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict


#define bottom undefined


glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = glBindVertexArrayOES


type VaoId = GLuint
type ShaderId = GLuint

data BuildDataVAO = BuildDataVAO
	{ vshaderId :: ShaderId
	, byteCount :: Int
	, byteMax :: Int
	, postShader :: IO ()
	}

emptyBuildVao :: ShaderId -> Int -> BuildDataVAO
emptyBuildVao i s = BuildDataVAO
		{ vshaderId = i
		, byteCount = 0
		, byteMax = s
		, postShader = return ()
		}

newtype BuildDataVAOT m a = BuildDataVAOT { unBuildVAOT :: StateT BuildDataVAO m a }
	deriving
		(Functor, Applicative, Monad, MonadIO)

instance MonadTrans BuildDataVAOT where
	lift = BuildDataVAOT . lift


runAttribute :: Monad m => ShaderId -> Int -> BuildDataVAOT m a -> m a
runAttribute s b (BuildDataVAOT m) = evalStateT m $ emptyBuildVao s b

class (Monad m) => BuildVAO m where
	stateVao :: (BuildDataVAO -> (a, BuildDataVAO)) -> m a

instance Monad m => BuildVAO (BuildDataVAOT m) where
	stateVao = BuildDataVAOT . state

modifyVao :: BuildVAO m => (BuildDataVAO -> BuildDataVAO) -> m ()
modifyVao f = stateVao (\s -> ((), f s))

getsVao :: BuildVAO m => (BuildDataVAO -> r) -> m r
getsVao f = f <$> stateVao (\s -> (s, s))

setByteMax :: BuildVAO m => Int -> m ()
setByteMax i = modifyVao $ \s -> s { byteMax = i }

advanceBy :: (Monad m, BuildVAO m, Storable s) => s -> m Int
advanceBy a = do
	i <- getsVao byteCount
	modifyVao $ \s -> s { byteCount = sizeOf a + byteCount s }
	return i

nameVao :: (Monad m, BuildVAO m, GLtype a) => a -> m String
nameVao a = do
	b <- getsVao byteCount
	return $ "a" ++ show b ++ glShortName a



-- Make VAO ------------------------------------------------------------------------------

setAttributes :: (Attribute a b, MonadIO m) => ShaderId -> a -> m (VaoId, b, IO ())
setAttributes s a = runAttribute s (sizeOf a) $ do
	i <- glGenVertexArray
	glBindVertexArray i
	e <- setAttribute a
	ps <- getsVao postShader
	return (i, e, ps)

attributesNoReg :: Monad m => Attribute a b => a -> m b
attributesNoReg a = runAttribute 0 (sizeOf a) $ setAttribute a


setupAttribute1
	:: (BuildVAO m, Monad m, GLtype a, Storable a) => a -> m (Expr V a)
setupAttribute1 a = do
	s <- getsVao vshaderId
	n <- nameVao a
	maxSize <- getsVao byteMax
	o <- advanceBy a
	let ps = withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c
		when (p < 2^15-1) $ do
			glVertexAttribPointer p
				(glComponents a)
				(glType a)
				(glNormalized a)
				(itoi $ maxSize)
				(intPtrToPtr $ IntPtr o)
			glEnableVertexAttribArray p
	modifyVao $ \s' -> s' { postShader = postShader s' >> ps }
		-- ~ liftIO $ putStrLn $ "sl pos: " ++ show p ++ "\t arr pos: " ++ show o ++ "\t stride: " ++ (show $ itoi $ maxSize - sizeOf a) ++ "\t components: " ++ (show $ glComponents a)
	return $ Expr $ ExprI n (toTypeS a) [] RegisterVertex



class Storable a => Attribute a b | a -> b, b -> a where
	setAttribute :: (BuildVAO m) => a -> m b

instance Attribute Bool (Expr V Bool) where setAttribute = setupAttribute1
instance Attribute Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance Attribute Float (Expr V Float) where setAttribute = setupAttribute1

instance (Attribute a c, Attribute b d) => Attribute (a,b) (c,d) where
	setAttribute _ = liftM2 (,) (setAttribute (bottom :: a)) (setAttribute (bottom :: b))

instance (Attribute a x, Attribute b y, Attribute c z) => Attribute (a,b,c) (x,y,z) where
	setAttribute _ = liftM3 (,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))

instance (Attribute a x, Attribute b y, Attribute c z, Attribute d w) =>
	Attribute (a,b,c,d) (x,y,z,w) where
	setAttribute _ = liftM4 (,,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))
		(setAttribute (bottom :: d))

-- would require more Storable instances
-- ~ instance (Attribute a x, Attribute b y, Attribute c z, Attribute d w, Attribute e v) =>
	-- ~ Attribute (a,b,c,d,e) (x,y,z,w,v) where
	-- ~ setAttribute _ = liftM4 (,,,,)
		-- ~ (setAttribute (bottom :: a))
		-- ~ (setAttribute (bottom :: b))
		-- ~ (setAttribute (bottom :: c))
		-- ~ (setAttribute (bottom :: d))
		-- ~ (setAttribute (bottom :: e))

attribPartsVec ::
	( BuildVAO m, Monad m, GLtype a, Storable a
	, GLtype a, GLtype (v a), Storable a, Storable (v a), Vector v)
	=> v a -> m (v (Expr V a))
attribPartsVec a = vecParts <$> setupAttribute1 a

instance Attribute (V2 Float) (V2 (Expr V Float)) where setAttribute = attribPartsVec
instance Attribute (V2 Int32) (V2 (Expr V Int32)) where setAttribute = attribPartsVec
instance Attribute (V2 Bool)  (V2 (Expr V Bool)) where setAttribute = attribPartsVec

instance Attribute (V3 Float) (V3 (Expr V Float)) where setAttribute = attribPartsVec
instance Attribute (V3 Int32) (V3 (Expr V Int32)) where setAttribute = attribPartsVec
instance Attribute (V3 Bool)  (V3 (Expr V Bool)) where setAttribute = attribPartsVec

instance Attribute (V4 Float) (V4 (Expr V Float)) where setAttribute = attribPartsVec
instance Attribute (V4 Int32) (V4 (Expr V Int32)) where setAttribute = attribPartsVec
instance Attribute (V4 Bool)  (V4 (Expr V Bool)) where setAttribute = attribPartsVec


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

