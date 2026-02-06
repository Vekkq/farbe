{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}


module Graphics.Farbe.GLFunc where


import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Utility
import Data.Composition


import Control.Monad.IO.Class
import Control.Monad.Trans

import Foreign hiding (void)

import Graphics.GL.Embedded20
	( pattern GL_FALSE, pattern GL_TRUE, pattern GL_INT
	, pattern GL_FLOAT, pattern GL_BOOL)
import qualified Graphics.GL.Embedded20 as GL
import Graphics.GL.Ext.OES.VertexArrayObject as GLEXT
-- ~ import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

import Graphics.GL.Ext.OES.GetProgramBinary as GL



class MonadIO m => GL m where
	glGenBuffers :: GLsizei -> Ptr GLuint -> m ()
	glBindBuffer :: GLenum -> GLuint -> m ()
	glBufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
	glBufferSubData :: GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
	glDeleteBuffers :: GLsizei -> Ptr GLuint -> m ()
	glGenVertexArraysOES :: GLsizei -> Ptr GLuint -> m ()
	glBindVertexArrayOES :: GLuint -> m ()
	glDrawArrays :: GLenum -> GLint -> GLsizei -> m ()
	glVertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> m ()
	glUniform1f :: GLint -> GLfloat -> m ()
	glUniform2f :: GLint -> GLfloat -> GLfloat -> m ()
	glUniform3f :: GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform4f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform1i :: GLint -> GLint -> m ()
	glUniform2i :: GLint -> GLint -> GLint -> m ()
	glUniform3i :: GLint -> GLint -> GLint -> GLint -> m ()
	glUniform4i :: GLint -> GLint -> GLint -> GLint -> GLint -> m ()
	glUniformMatrix3fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniformMatrix4fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glGenTextures :: GLsizei -> Ptr GLuint -> m ()
	glActiveTexture :: GLenum -> m ()
	glBindTexture :: GLenum -> GLuint -> m ()
	glDeleteTextures :: GLsizei -> Ptr GLuint -> m ()
	glTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
	glTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
	glClear :: GLbitfield -> m ()
	glClearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glReadPixels :: GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
	glBindFramebuffer :: GLenum -> GLuint -> m ()
	glBindRenderbuffer :: GLenum -> GLuint -> m ()
	glCheckFramebufferStatus :: GLenum -> m GLenum
	glDeleteFramebuffers :: GLsizei -> Ptr GLuint -> m ()
	glFramebufferRenderbuffer :: GLenum -> GLenum -> GLenum -> GLuint -> m ()
	glFramebufferTexture2D :: GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
	glGenFramebuffers :: GLsizei -> Ptr GLuint -> m ()
	glGenRenderbuffers :: GLsizei -> Ptr GLuint -> m ()
	glGenerateMipmap :: GLenum -> m ()
	glTexParameteri :: GLenum -> GLenum -> GLint -> m ()
	glCreateProgram :: m GLuint
	glBindAttribLocation :: GLuint -> GLuint -> Ptr GLchar -> m ()
	glLinkProgram :: GLuint -> m ()
	glGetShaderSource :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
	glGetProgramBinaryOES :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> m ()
	glProgramBinaryOES :: GLuint -> GLenum -> Ptr () -> GLint -> m ()


instance MonadIO m => GL (GLAction m) where
	glCreateProgram = lift GL.glCreateProgram
	glGenBuffers = lift .: GL.glGenBuffers
	glGenTextures = lift .: GL.glGenTextures
	glGenFramebuffers = lift .: GL.glGenFramebuffers
	glBindBuffer = defer .: GL.glBindBuffer
	-- ~ glBindBuffer :: GLenum -> GLuint -> m ()
	-- ~ glBufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
	-- ~ glBufferSubData :: GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
	-- ~ glDeleteBuffers :: GLsizei -> Ptr GLuint -> m ()
	-- ~ glGenVertexArraysOES :: GLsizei -> Ptr GLuint -> m ()
	-- ~ glBindVertexArrayOES :: GLuint -> m ()
	-- ~ glDrawArrays :: GLenum -> GLint -> GLsizei -> m ()
	-- ~ glVertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> m ()
	-- ~ glUniform1f :: GLint -> GLfloat -> m ()
	-- ~ glUniform2f :: GLint -> GLfloat -> GLfloat -> m ()
	-- ~ glUniform3f :: GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
	-- ~ glUniform4f :: GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	-- ~ glUniform1i :: GLint -> GLint -> m ()
	-- ~ glUniform2i :: GLint -> GLint -> GLint -> m ()
	-- ~ glUniform3i :: GLint -> GLint -> GLint -> GLint -> m ()
	-- ~ glUniform4i :: GLint -> GLint -> GLint -> GLint -> GLint -> m ()
	-- ~ glUniformMatrix3fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	-- ~ glUniformMatrix4fv :: GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	-- ~ glActiveTexture :: GLenum -> m ()
	-- ~ glBindTexture :: GLenum -> GLuint -> m ()
	-- ~ glDeleteTextures :: GLsizei -> Ptr GLuint -> m ()
	-- ~ glTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> m ()
	-- ~ glTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
	-- ~ glClear :: GLbitfield -> m ()
	-- ~ glClearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	-- ~ glReadPixels :: GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
	-- ~ glBindFramebuffer :: GLenum -> GLuint -> m ()
	-- ~ glBindRenderbuffer :: GLenum -> GLuint -> m ()
	-- ~ glCheckFramebufferStatus :: GLenum -> m GLenum
	-- ~ glDeleteFramebuffers :: GLsizei -> Ptr GLuint -> m ()
	-- ~ glFramebufferRenderbuffer :: GLenum -> GLenum -> GLenum -> GLuint -> m ()
	-- ~ glFramebufferTexture2D :: GLenum -> GLenum -> GLenum -> GLuint -> GLint -> m ()
	-- ~ glGenRenderbuffers :: GLsizei -> Ptr GLuint -> m ()
	-- ~ glGenerateMipmap :: GLenum -> m ()
	-- ~ glTexParameteri :: GLenum -> GLenum -> GLint -> m ()
	-- ~ glBindAttribLocation :: GLuint -> GLuint -> Ptr GLchar -> m ()
	-- ~ glLinkProgram :: GLuint -> m ()
	-- ~ glGetShaderSource :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
	-- ~ glGetProgramBinaryOES :: GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> m ()
	-- ~ glProgramBinaryOES :: GLuint -> GLenum -> Ptr () -> GLint -> m ()



type GLAction = DeferT IO


getGLAction :: GLAction m () -> m (IO ())
getGLAction = undefined






