{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe.ToMain where


import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.Chan



-- ~ import Graphics.Farbe.GL
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import Foreign hiding (void)
import Graphics.GL.Types
-- ~ import Graphics.GL.Embedded20
	-- ~ ( pattern GL_FALSE, pattern GL_TRUE, pattern GL_INT
	-- ~ , pattern GL_FLOAT, pattern GL_BOOL)
import qualified Graphics.GL.Embedded20 as GL
import Graphics.GL.Ext.OES.VertexArrayObject as GLEXT


newtype ToMainT a = ToMainT { unToMain :: ReaderT (Chan (IO ())) IO a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadIO
		)

class ToMain m where
	toMain :: IO () -> m ()

instance ToMain ToMainT where
	toMain io = do
		chan <- ToMainT ask
		liftIO $ writeChan chan io

toMainReturn :: (ToMain m, MonadIO m) => IO a -> m a
toMainReturn m = do
	v <- liftIO $ newEmptyMVar
	toMain $ m >>= putMVar v
	liftIO $ readMVar v

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(toMain,ToMain,.)

runToMain :: ToMainT () -> IO ()
runToMain m = do
	chan <- newChan
	forkIO $ void $ runReaderT (unToMain m) chan
	void $ getChanContents chan >>= sequence






class GL m where
	glGenBuffers :: GLsizei -> Ptr GLuint -> m ()
	glBindBuffer :: GLenum -> GLuint -> m ()
	glBufferData :: GLenum -> GLsizeiptr -> Ptr () -> GLenum -> m ()
	glBufferSubData :: GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> m ()
	glGenVertexArraysOES :: GLsizei -> Ptr GLuint -> m ()
	glBindVertexArrayOES :: GLuint -> m ()
	glCreateProgram :: MonadIO m => m GLuint
	glAttachShader :: MonadIO m => GLuint -> GLuint -> m ()
	glGetAttachedShaders :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> m ()
	glDeleteShader :: MonadIO m => GLuint -> m ()
	glBindAttribLocation :: MonadIO m => GLuint -> GLuint -> Ptr GLchar -> m ()
	glLinkProgram :: MonadIO m => GLuint -> m ()
	glGetUniformLocation :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
	glUseProgram :: MonadIO m => GLuint -> m ()
	glUniform1f :: MonadIO m => GLint -> GLfloat -> m ()
	glUniform2f :: MonadIO m => GLint -> GLfloat -> GLfloat -> m ()
	glUniform3f :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform4f :: MonadIO m => GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
	glUniform1i :: MonadIO m => GLint -> GLint -> m ()
	glUniform2i :: MonadIO m => GLint -> GLint -> GLint -> m ()
	glUniform3i :: MonadIO m => GLint -> GLint -> GLint -> GLint -> m ()
	glUniform4i :: MonadIO m => GLint -> GLint -> GLint -> GLint -> GLint -> m ()
	glUniformMatrix3fv :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glUniformMatrix4fv :: MonadIO m => GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> m ()
	glGetAttribLocation :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
	glGenTextures :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
	glActiveTexture :: MonadIO m => GLenum -> m ()
	glDeleteTextures :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
	glBindTexture :: MonadIO m => GLenum -> GLuint -> m ()


instance GL (ToMainT) where
	glGenBuffers = toMain .** GL.glGenBuffers
	glBindBuffer = toMain .** GL.glBindBuffer
	glBufferData = toMain .**** GL.glBufferData
	glBufferSubData = toMain .**** GL.glBufferSubData
	glGenVertexArraysOES = toMain .** GLEXT.glGenVertexArraysOES
	glBindVertexArrayOES = toMain . GLEXT.glBindVertexArrayOES
	glCreateProgram = toMainReturn $ GL.glCreateProgram
	glAttachShader = toMain .** GL.glAttachShader
	glGetAttachedShaders = toMain .**** GL.glGetAttachedShaders
	glDeleteShader = toMain . GL.glDeleteShader
	glBindAttribLocation = toMain .*** GL.glBindAttribLocation
	glLinkProgram = toMain . GL.glLinkProgram
	glGetUniformLocation = toMainReturn .** GL.glGetUniformLocation
	glUseProgram = toMain . GL.glUseProgram
	glUniform1f = toMain .** GL.glUniform1f
	glUniform2f = toMain .*** GL.glUniform2f
	glUniform3f = toMain .**** GL.glUniform3f
	glUniform4f = toMain .***** GL.glUniform4f
	glUniform1i = toMain .** GL.glUniform1i
	glUniform2i = toMain .*** GL.glUniform2i
	glUniform3i = toMain .**** GL.glUniform3i
	glUniform4i = toMain .***** GL.glUniform4i
	glUniformMatrix3fv = toMain .**** GL.glUniformMatrix3fv
	glUniformMatrix4fv = toMain .**** GL.glUniformMatrix4fv
	glGetAttribLocation = toMainReturn .** GL.glGetAttribLocation
	glGenTextures = toMain .** GL.glGenTextures
	glActiveTexture = toMain . GL.glActiveTexture
	glDeleteTextures = toMain .** GL.glDeleteTextures
	glBindTexture = toMain .** GL.glBindTexture


(.**) = (.) . (.)

(.***) = (.) . (.**)
(.****) = (.) . (.***)
(.*****) = (.) . (.****)




