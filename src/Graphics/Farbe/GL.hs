{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}


module Graphics.Farbe.GL
	( module Graphics.GL.Types
	, Hashable
	, GL (..)
	-- ~ , GLDebug (..)
	, TypeS (..)
	, GLtype (..)
	, Normalized (..)
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
	, pattern GL.GL_VERTEX_SHADER
	, pattern GL.GL_FRAGMENT_SHADER
	, pattern GL.GL_COMPILE_STATUS
	, pattern GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
	, pattern GL.GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
	, pattern GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
	, pattern GL.GL_FRAMEBUFFER_UNSUPPORTED
	, pattern GL.GL_DEPTH_TEST
	, pattern GL.GL_UNPACK_ALIGNMENT
	, pattern GL.GL_DEPTH_ATTACHMENT
	, pattern GL.GL_COLOR_BUFFER_BIT
	, pattern GL.GL_DEPTH_BUFFER_BIT
	, pattern GL.GL_STENCIL_TEST
	, pattern GL.GL_NEAREST
	, pattern GL.GL_LESS
	, pattern GL.GL_GREATER
	, pattern GL.GL_ALWAYS
	, pattern GL.GL_KEEP
	, pattern GL.GL_DECR_WRAP
	, pattern GL.GL_FRAMEBUFFER
	, pattern GL.GL_COLOR_ATTACHMENT0
	, pattern GL.GL_TEXTURE_MIN_FILTER
	, pattern GL.GL_TEXTURE_MAG_FILTER
	, pattern GL.GL_STENCIL_BUFFER_BIT
	, boolToInt
	) where




import Graphics.Farbe.Vec
-- ~ import Graphics.Farbe.Tuple

import GHC.Generics (Generic)
import Data.Hashable

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
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
	glDebug :: m Bool
	glDebug = return True

	whenGLDebug :: m () -> m ()
	whenGLDebug m = do
		b <- glDebug
		when b m

	whenGLDebug' :: IO () -> m ()
	whenGLDebug' m = whenGLDebug (liftIO m)

	glGenBuffers :: GL m => GLsizei -> Ptr GLuint -> m ()
	glGenBuffers = GL.glGenBuffers
	glBindBuffer :: GL m => GLenum -> GLuint -> m ()
	glBindBuffer = GL.glBindBuffer
	glBufferData :: GL m => GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
	glBufferData = GL.glBufferData
	glBufferSubData :: GL m => GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
	glBufferSubData = GL.glBufferSubData
	glGenVertexArraysOES :: GL m => GLsizei -> Ptr GLuint -> m ()
	glGenVertexArraysOES = GLEXT.glGenVertexArraysOES
	glBindVertexArrayOES :: GL m => GLuint -> m ()
	glBindVertexArrayOES = GLEXT.glBindVertexArrayOES
	glGetBufferPointervOES :: GL m => GLenum -> GLenum -> Ptr (Ptr ()) -> m ()
	glGetBufferPointervOES = GLEXT.glGetBufferPointervOES
	glDeleteBuffers :: GL m => GLsizei -> Ptr GLuint -> m ()
	glDeleteBuffers = GL.glDeleteBuffers
	glDrawArrays :: GL m => GLenum -> GLint -> GLsizei -> m ()
	glDrawArrays = GL.glDrawArrays
	glCreateShader :: GL m => GLenum -> m GLuint
	glCreateShader = GL.glCreateShader
	glCompileShader :: GL m => GLuint -> m ()
	glCompileShader = GL.glCompileShader
	glAttachShader :: GL m => GLuint -> GLuint -> m ()
	glAttachShader = GL.glAttachShader
	glShaderSource :: GL m => GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> m ()
	glShaderSource = GL.glShaderSource
	glUseProgram :: GL m => GLuint -> m ()
	glUseProgram = GL.glUseProgram
	glGetShaderInfoLog :: GL m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
	glGetShaderInfoLog = GL.glGetShaderInfoLog
	glGetShaderiv :: GL m => GLuint -> GLenum -> Ptr GLint -> m ()
	glGetShaderiv = GL.glGetShaderiv
	glCreateProgram :: GL m => m GLuint
	glCreateProgram = GL.glCreateProgram
	glBindAttribLocation :: GL m => GLuint -> GLuint -> Ptr GLchar -> m ()
	glBindAttribLocation = GL.glBindAttribLocation
	glLinkProgram :: GL m => GLuint -> m ()
	glLinkProgram = GL.glLinkProgram
	glGetUniformLocation :: GL m => GLuint -> Ptr GLchar -> m GLint
	glGetUniformLocation = GL.glGetUniformLocation
	glUniform1f :: GL m => GLint -> GLfloat -> m ()
	glUniform1f = GL.glUniform1f
	glUniform2f :: GL m => GLint -> GLfloat -> GLfloat -> m ()
	glUniform2f = GL.glUniform2f
	glUniform3f :: GL m => GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform3f = GL.glUniform3f
	glUniform4f :: GL m => GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform4f = GL.glUniform4f
	glUniform1i :: GL m => GLint -> GLint -> m ()
	glUniform1i = GL.glUniform1i
	glUniform2i :: GL m => GLint -> GLint -> GLint -> m ()
	glUniform2i = GL.glUniform2i
	glUniform3i :: GL m => GLint -> GLint -> GLint -> GLint -> m ()
	glUniform3i = GL.glUniform3i
	glUniform4i :: GL m => GLint -> GLint -> GLint -> GLint -> GLint -> m ()
	glUniform4i = GL.glUniform4i
	glUniformMatrix3fv :: GL m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniformMatrix3fv = GL.glUniformMatrix3fv
	glUniformMatrix4fv :: GL m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniformMatrix4fv = GL.glUniformMatrix4fv
	glGetIntegerv :: GL m => GLenum -> Ptr GLint -> m ()
	glGetIntegerv = GL.glGetIntegerv
	glGenTextures :: GL m => GLsizei -> Ptr GLuint -> m ()
	glGenTextures = GL.glGenTextures
	glActiveTexture :: GL m => GLenum -> m ()
	glActiveTexture e = do
		GL.glActiveTexture e
		whenGLDebug' $ putStrLn $ "glActiveTexture " ++ show (e - GL.GL_TEXTURE0)
	glBindTexture :: GL m => GLenum -> GLuint -> m ()
	glBindTexture e i = do
		GL.glBindTexture e i
		whenGLDebug' $ putStrLn $ "glBindTexture " ++ show i
	glTexImage2D :: GL m => GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
	glTexImage2D = GL.glTexImage2D
	glGenerateMipmap :: GL m => GLenum -> m ()
	glGenerateMipmap = GL.glGenerateMipmap
	glDeleteTextures :: GL m => GLsizei -> Ptr GLuint -> m ()
	glDeleteTextures i p = do
		liftIO $ peek p >>= \a -> putStrLn $ "glDeleteTextures " ++ show a
		GL.glDeleteTextures i p
	glClearColor :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glClearColor = GL.glClearColor
	glEnable :: MonadIO m => GLenum -> m ()
	glEnable = GL.glEnable
	glDisable :: MonadIO m => GLenum -> m ()
	glDisable = GL.glDisable
	glPixelStorei :: MonadIO m => GLenum -> GLint -> m ()
	glPixelStorei = GL.glPixelStorei
	glGetError :: MonadIO m => m GLenum
	glGetError = GL.glGetError
	glStencilFunc :: MonadIO m => GLenum -> GLint -> GLuint -> m ()
	glStencilFunc = GL.glStencilFunc
	glClear :: MonadIO m => GLbitfield -> m ()
	glClear = GL.glClear
	glColorMask :: MonadIO m => GLboolean -> GLboolean -> GLboolean -> GLboolean -> m ()
	glColorMask = GL.glColorMask
	glStencilOp :: MonadIO m => GLenum -> GLenum -> GLenum -> m ()
	glStencilOp = GL.glStencilOp
	glFramebufferTexture2D :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
	glFramebufferTexture2D = GL.glFramebufferTexture2D
	glTexParameteri :: MonadIO m => GLenum -> GLenum -> GLint -> m ()
	glTexParameteri = GL.glTexParameteri
	glGenFramebuffers :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
	glGenFramebuffers = GL.glGenFramebuffers
	glBindFramebuffer :: MonadIO m => GLenum -> GLuint -> m ()
	glBindFramebuffer = GL.glBindFramebuffer
	glCheckFramebufferStatus :: MonadIO m => GLenum -> m GLenum
	glCheckFramebufferStatus = GL.glCheckFramebufferStatus


instance GL IO


-- ~ class ShowFn f where
	-- ~ showFn :: MonadIO m => f -> m String

-- ~ instance Show a => ShowFn (a -> r) where
	-- ~ showFn f = \a -> fmap (show a++) $ showFn (f a)


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










