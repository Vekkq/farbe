{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Start of a module for swapping the entire GL backend, where necessary
-- very unfinished
-- a separate backend module might still be better

-- ~ module Graphics.Farbe.I.GL where
module Graphics.Farbe.GL where


import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple

import Control.Monad.IO.Class
import Foreign hiding (void)
import Foreign.C
import GHC.TypeNats
import Data.Proxy
import Data.Kind

-- ~ import Graphics.GL.Embedded20 (GLsizei, GLuint, GLint)
import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

-- tl;dr use gl types directly


class GL m where
  genBuffers :: GLsizei -> Ptr GLuint -> m ()
  bindBuffer :: GLenum -> GLuint -> m ()
  bufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
  bufferSubData :: GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
  genVertexArraysOES :: GLsizei -> Ptr GLuint -> m ()
  bindVertexArrayOES :: GLuint -> m ()


instance MonadIO m => GL m where
  genBuffers = glGenBuffers
  bindBuffer = glBindBuffer
  bufferData = glBufferData
  bufferSubData = glBufferSubData
  genVertexArraysOES = glGenVertexArraysOES
  bindVertexArrayOES = glBindVertexArrayOES


-- ~ class GLRaw m => GL m where
  -- ~ genBuffer :: m GLuint
  -- ~ genBuffer = do
    -- ~ i <- withPtr_ $ glGenBuffers 1
    -- ~ glBindBuffer GL_ARRAY_BUFFER v
    -- ~ glBufferData GL_ARRAY_BUFFER newSize p GL_STATIC_DRAW

-- ~ instance GL IO where
  -- ~ genBuffer = withPtr_ $ glGenBuffers 1
  -- ~ bindBuffer = glBindBuffer GL_ARRAY_BUFFER
  -- ~ bufferData = glBufferData GL_ARRAY_BUFFER s nullPtr GL_STATIC_DRAW


-- GL type information -------------------------------------------------------------------

data TypeS = TBool | TInt | TFloat | TVec2 TypeS | TVec3 TypeS | TVec4 TypeS | TTex


class Eq a => GLtype a where
	slName :: a -> String
	fType :: a -> TypeS
	glType :: a -> GLenum
	glComponents :: a -> GLint
	glComponents _ = 1
	glNormalized :: a -> GLboolean
	glNormalized _ = GL_FALSE
	glShortName :: a -> String
	glShortName a = take 1 $ slName a
	glPrecision :: a -> String
	glPrecision _ = "highp"
	slNameWithPrec :: a -> String
	slNameWithPrec a = glPrecision a ++ " " ++ slName a

instance GLtype Bool where
	slName _ = "bool"
	fType _ = TBool
	glType _ = GL_BOOL

instance GLtype Int32 where
	slName _ = "int"
	fType _ = TInt
	glType _ = GL_INT

instance GLtype Float where
	slName _ = "float"
	fType _ = TFloat
	glType _ = GL_FLOAT

instance GLtype (V2 Float) where
	slName _ = "vec2"
	fType _ = TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 2

instance GLtype (V3 Float) where
	slName _ = "vec3"
	fType _ = TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 3

instance GLtype (V4 Float) where
	slName _ = "vec4"
	fType _ = TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4


instance GLtype (V2 Int32) where
	slName _ = "ivec2"
	fType _ = TVec2 TInt
	glType _ = GL_INT
	glComponents _ = 2

instance GLtype (V3 Int32) where
	slName _ = "ivec3"
	fType _ = TVec3 TInt
	glType _ = GL_INT
	glComponents _ = 3

instance GLtype (V4 Int32) where
	slName _ = "ivec4"
	fType _ = TVec4 TInt
	glType _ = GL_INT
	glComponents _ = 4

instance GLtype (V2 Bool) where
	slName _ = "bvec2"
	fType _ = TVec2 TBool
	glType _ = GL_BOOL
	glComponents _ = 2

instance GLtype (V3 Bool) where
	slName _ = "bvec3"
	fType _ = TVec3 TBool
	glType _ = GL_BOOL
	glComponents _ = 3

instance GLtype (V4 Bool) where
	slName _ = "bvec4"
	fType _ = TVec4 TBool
	glType _ = GL_BOOL
	glComponents _ = 4

boolToInt :: Bool -> Int32
boolToInt True = 1
boolToInt _ = 0


instance GLtype (Mat V2 V2 Float) where
	slName _ = "mat2"
	fType _ = TVec2 $ TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4

instance GLtype (Mat V3 V3 Float) where
	slName _ = "mat3"
	fType _ = TVec3 $ TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 9

instance GLtype (Mat V4 V4 Float) where
	slName _ = "mat4"
	fType _ = TVec4 $ TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 16

withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray

(.:) = (.).(.)

data Normalized a = Normalized { unNormalized :: a } deriving (Eq)

instance Functor Normalized where
	fmap f (Normalized a) = Normalized $ f a

instance Storable a => Storable (Normalized a) where
	sizeOf _ = sizeOf (err :: a)
	alignment _ = alignment (err :: a)
	peek p = fmap Normalized $ peek $ castPtr p
	poke p (Normalized a) = poke (castPtr p) a

instance GLtype a => GLtype (Normalized a) where
	glNormalized _ = GL_TRUE
	slName _ = slName (err :: a)
	fType _ = fType (err :: a)
	glType _ = glType (err :: a)
	glComponents _ = glComponents (err :: a)
	-- ~ setupUpload l (Normalized e) = setupUpload l e













