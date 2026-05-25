{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}


module Graphics.Farbe.GL
	( module Graphics.GL.Types
	, Hashable
	, GL (..)
	, TypeS (..)
	, GLtype (..)
	, pattern GL.GL_FALSE
	, pattern GL.GL_TRUE
	, pattern GL.GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	, pattern GL.GL_LUMINANCE
	, pattern GL.GL_LUMINANCE_ALPHA
	, pattern GL.GL_ALPHA
	, pattern GL.GL_RGB
	, pattern GL.GL_RGBA
	, pattern GL.GL_DEPTH_COMPONENT
	, pattern GL.GL_UNSIGNED_BYTE
	, pattern GL.GL_TEXTURE_2D
	, pattern GL.GL_TEXTURE0
	, pattern GL.GL_INT
	, boolToInt
	) where


import Graphics.Farbe.Vec
-- ~ import Graphics.Farbe.Tuple

import GHC.Generics (Generic)
import Data.Hashable

import Control.Monad.IO.Class
import Foreign hiding (void)

import Graphics.GL.Embedded20
	( pattern GL_FALSE, pattern GL_TRUE, pattern GL_INT
	, pattern GL_FLOAT, pattern GL_BOOL)
import qualified Graphics.GL.Embedded20 as GL
import Graphics.GL.Ext.OES.VertexArrayObject as GLEXT
import Graphics.GL.Ext.OES.Mapbuffer as GLEXT
import Graphics.GL.Types

-- tl;dr use gl types directly





class MonadIO m => GL m where
	glGenBuffers :: GLsizei -> Ptr GLuint -> m ()
	glBindBuffer :: GLenum -> GLuint -> m ()
	glBufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
	glBufferSubData :: GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
	glGenVertexArraysOES :: GLsizei -> Ptr GLuint -> m ()
	glBindVertexArrayOES :: GLuint -> m ()
	glGetBufferPointervOES :: MonadIO m => GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
	glDeleteBuffers :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
	glDrawArrays :: MonadIO m => GLenum -> GLint -> GLsizei -> m ()
	glCreateProgram :: MonadIO m => m GLuint
	glBindAttribLocation :: MonadIO m => GLuint -> GLuint -> Ptr GLchar -> m ()
	glLinkProgram :: MonadIO m => GLuint -> m ()
	glGetUniformLocation :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
	glUniform1f :: MonadIO m => GLint -> GLfloat -> m ()
	glUniform2f :: MonadIO m => GLint -> GLfloat -> GLfloat -> m ()
	glUniform3f :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform4f :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniformMatrix3fv :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniformMatrix4fv :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniform1i :: MonadIO m => GLint -> GLint -> m ()
	glUniform2i :: MonadIO m => GLint -> GLint -> GLint -> m ()
	glUniform3i :: MonadIO m => GLint -> GLint -> GLint -> GLint -> m ()
	glUniform4i :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> m ()
	glGetIntegerv :: MonadIO m => GLenum -> Ptr GLint -> m ()
	glGenTextures :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
	glActiveTexture :: MonadIO m => GLenum -> m ()
	glBindTexture :: MonadIO m => GLenum -> GLuint -> m ()
	glTexImage2D :: MonadIO m => GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
	glGenerateMipmap :: MonadIO m => GLenum -> m ()
	glDeleteTextures :: MonadIO m => GLsizei -> Ptr GLuint -> m ()

instance GL IO where
	glGenBuffers = GL.glGenBuffers
	glBindBuffer = GL.glBindBuffer
	glBufferData = GL.glBufferData
	glBufferSubData = GL.glBufferSubData
	glGenVertexArraysOES = GLEXT.glGenVertexArraysOES
	glBindVertexArrayOES = GLEXT.glBindVertexArrayOES
	glGetBufferPointervOES = GLEXT.glGetBufferPointervOES
	glDeleteBuffers = GL.glDeleteBuffers
	glDrawArrays = GL.glDrawArrays
	glCreateProgram = GL.glCreateProgram
	glBindAttribLocation = GL.glBindAttribLocation
	glLinkProgram = GL.glLinkProgram
	glGetUniformLocation = GL.glGetUniformLocation
	glUniform1f = GL.glUniform1f
	glUniform2f = GL.glUniform2f
	glUniform3f = GL.glUniform3f
	glUniform4f = GL.glUniform4f
	glUniformMatrix3fv = GL.glUniformMatrix3fv
	glUniformMatrix4fv = GL.glUniformMatrix4fv
	glUniform1i = GL.glUniform1i
	glUniform2i = GL.glUniform2i
	glUniform3i = GL.glUniform3i
	glUniform4i = GL.glUniform4i
	glGetIntegerv = GL.glGetIntegerv
	glGenTextures = GL.glGenTextures
	glActiveTexture = GL.glActiveTexture
	glBindTexture = GL.glBindTexture
	glTexImage2D = GL.glTexImage2D
	glGenerateMipmap = GL.glGenerateMipmap
	glDeleteTextures = GL.glDeleteTextures



newtype GLDebug a = GLDebug { glDebug :: IO a }
	deriving (Functor, Applicative, Monad, MonadIO)






-- GL type information -------------------------------------------------------------------

data TypeS = TBool | TInt | TFloat | TVec2 TypeS | TVec3 TypeS | TVec4 TypeS | TTex
	deriving (Eq, Ord, Read, Show, Generic)

instance Hashable TypeS


class (Eq a) => GLtype a where
	slName :: a -> String
	toTypeS :: a -> TypeS
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
	toTypeS _ = TBool
	glType _ = GL_BOOL

instance GLtype Int32 where
	slName _ = "int"
	toTypeS _ = TInt
	glType _ = GL_INT

instance GLtype Float where
	slName _ = "float"
	toTypeS _ = TFloat
	glType _ = GL_FLOAT

instance GLtype (V2 Float) where
	slName _ = "vec2"
	toTypeS _ = TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 2
	glShortName _ = "v2"

instance GLtype (V3 Float) where
	slName _ = "vec3"
	toTypeS _ = TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 3
	glShortName _ = "v3"

instance GLtype (V4 Float) where
	slName _ = "vec4"
	toTypeS _ = TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4
	glShortName _ = "v4"


instance GLtype (V2 Int32) where
	slName _ = "ivec2"
	toTypeS _ = TVec2 TInt
	glType _ = GL_INT
	glComponents _ = 2
	glShortName _ = "v2i"

instance GLtype (V3 Int32) where
	slName _ = "ivec3"
	toTypeS _ = TVec3 TInt
	glType _ = GL_INT
	glComponents _ = 3
	glShortName _ = "v3i"

instance GLtype (V4 Int32) where
	slName _ = "ivec4"
	toTypeS _ = TVec4 TInt
	glType _ = GL_INT
	glComponents _ = 4
	glShortName _ = "v4i"

instance GLtype (V2 Bool) where
	slName _ = "bvec2"
	toTypeS _ = TVec2 TBool
	glType _ = GL_BOOL
	glComponents _ = 2
	glShortName _ = "v2b"

instance GLtype (V3 Bool) where
	slName _ = "bvec3"
	toTypeS _ = TVec3 TBool
	glType _ = GL_BOOL
	glComponents _ = 3
	glShortName _ = "v3b"

instance GLtype (V4 Bool) where
	slName _ = "bvec4"
	toTypeS _ = TVec4 TBool
	glType _ = GL_BOOL
	glComponents _ = 4
	glShortName _ = "v4b"

boolToInt :: Bool -> Int32
boolToInt True = 1
boolToInt _ = 0


instance GLtype (Mat V2 V2 Float) where
	slName _ = "mat2"
	toTypeS _ = TVec2 $ TVec2 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 4
	glShortName _ = "m2"

instance GLtype (Mat V3 V3 Float) where
	slName _ = "mat3"
	toTypeS _ = TVec3 $ TVec3 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 9
	glShortName _ = "m3"

instance GLtype (Mat V4 V4 Float) where
	slName _ = "mat4"
	toTypeS _ = TVec4 $ TVec4 TFloat
	glType _ = GL_FLOAT
	glComponents _ = 16
	glShortName _ = "m4"



data Normalized a = Normalized { unNormalized :: a } deriving (Eq)

instance Functor Normalized where
	fmap f (Normalized a) = Normalized $ f a

#define bottom undefined

instance Storable a => Storable (Normalized a) where
	sizeOf _ = sizeOf (bottom :: a)
	alignment _ = alignment (bottom :: a)
	peek p = fmap Normalized $ peek $ castPtr p
	poke p (Normalized a) = poke (castPtr p) a

instance GLtype a => GLtype (Normalized a) where
	glNormalized _ = GL_TRUE
	slName _ = slName (bottom :: a)
	toTypeS _ = toTypeS (bottom :: a)
	glType _ = glType (bottom :: a)
	glComponents _ = glComponents (bottom :: a)
	glShortName _ = "n" ++ glShortName (bottom :: a)










