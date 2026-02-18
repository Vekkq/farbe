{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe
	( runFarbeT
	, Farbe (..)
	, newVArray
	, frame
	, compile'
	, Render
	, transfer
	, use
	, renderTexture
	, display
	, drawTexture
	, drawDepth
	, module Graphics.Farbe.Vec
	, module Graphics.Farbe.Expr
	, makeVar
	, makeVarF
	, makeVarI
	, makeVarB
	, makeVarV2F
	, makeVarV2I
	, makeVarV2B
	, makeVarV3F
	, makeVarV3I
	, makeVarV3B
	, makeVarV4F
	, makeVarV4I
	, makeVarV4B
	, makeVarM2
	, makeVarM3
	, makeVarM4
	, makeVarT

	) where

import Graphics.Farbe.Vec
import Graphics.Farbe.Shader
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Utils
import Graphics.Farbe.Delay
import Graphics.Farbe.Expr
import Graphics.Farbe.GL
import Graphics.Farbe.Window
import Control.Monad.IO.Class
import Data.Int
import Foreign.Storable
import Foreign.Ptr

import Control.Concurrent.MVar.Lifted

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS
import Graphics.Farbe.Utility

import Data.Map
import Data.Dynamic

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Graphics.Farbe.GLScheduler


data Config = Config
	{ debugMode :: Bool
	, devDebugMode :: Bool
	}

defaultConfig = Config { debugMode = True, devDebugMode = True }

newtype ConfigT m a = ConfigT { unConfigT :: ReaderT Config m a }
	deriving
		( Functor, Applicative, Monad, MonadIO
		, Count, HandTex, HandVBO
		, MonadState s, MonadWriter w
		, MonadError e, MonadWindow
		, DelayedState f
		, ShaderCache g
		)

runConfigT :: Config -> ConfigT m a -> m a
runConfigT c (ConfigT m) = runReaderT m c


newtype FarbeT m a = FarbeT { unFarbe :: DelayedT SmallWorld (ShaderCacheT (HandTexT IO) (CounterT (HandTexT (HandVBOT m)))) a }
	deriving
		( Functor, Applicative, Monad, MonadIO
		, Count, HandTex, HandVBO
		, MonadReader r, MonadState s, MonadWriter w
		, MonadError e, MonadWindow
		, DelayedState SmallWorld
		, ShaderCache (HandTexT IO)
		)

instance MonadTrans FarbeT where
	lift = FarbeT . lift . lift . lift . lift . lift

-- ~ deriving instance (Monad m) => Count (WindowT m)

class (Count m, HandTex m, HandVBO m, MonadWindow m, MonadIO m) => Farbe m
instance (MonadWindow m, MonadIO m) => Farbe (FarbeT m)

instance (Farbe m, Monad m) => Farbe (ReaderT r m)
instance (Farbe m, Monad m, Monoid w) => Farbe (WriterT w m)
instance (Farbe m, Monad m) => Farbe (StateT r m)
instance (Farbe m, Monad m) => Farbe (ExceptT r m)
instance (Farbe m, Monad m, Monoid w) => Farbe (RWST r w s m)

instance MonadIO m => MonadFail (FarbeT m) where
	fail s = liftIO $ do
		return $ error s


runFarbeT :: MonadIO m => FarbeT m a -> m a
runFarbeT m = runHandVBOT (2^24)
	. runHandTexT
	. runCounterT'
	. fmap fst . runShaderCache
	. fmap fst . runDelayedT
	. unFarbe $ do
		glClearColor 0.1 0.1 0.1 1
		glEnable GL_DEPTH_TEST
		glPixelStorei GL_UNPACK_ALIGNMENT 1
		-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE

		-- ~ glEnable GL_CULL_FACE
		m


-- ~ runFarbeT' :: MonadIO m => WindowT (FarbeT m) a -> m a
-- ~ runFarbeT' = runFarbeT . runWindowT "" (InWindow (1000,1024))



display :: Farbe m => Render m -> m ()
display r = do
	display' r
	swapBuffers

data Render m
	= DrawShader (m ())
	| DrawOver (Render m) (Render m)
	| DrawInto (Render m) (Render m)
	| Draws [(Render m)]

display' :: Farbe m => Render m -> m ()
display' (DrawShader m) = m
display' (Draws ms) = mapM_ display' ms
display' (DrawOver a b) = do
	glEnable GL_STENCIL_TEST
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_DECR_WRAP
	display' a
	glStencilFunc GL_GREATER 1 1
	display' b
	glDisable GL_STENCIL_TEST

display' (DrawInto a b) = do
	glEnable GL_STENCIL_TEST
	glClear GL_STENCIL_BUFFER_BIT
	glStencilOp GL_KEEP GL_DECR_WRAP GL_DECR_WRAP
	glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
	display' a
	glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

	glStencilOp GL_KEEP GL_KEEP GL_KEEP
	glStencilFunc GL_LESS 1 0xFF
	glDisable GL_DEPTH_TEST
	display' b

	glStencilFunc GL_ALWAYS 0 0xFF
	glDisable GL_STENCIL_TEST


nextFrame :: MonadWindow m => m ()
nextFrame = do

	swapBuffers





drawTexture :: Farbe m => m (Render m -> m (Texture RGB))
drawTexture = do
	(w',h') <- windowSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texRGB :: Texture RGB <- loadTexture2Base (w,h) nullPtr
	glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (texId texRGB) 0
	-- replace texture with renderbuffer in this function
	texD :: Texture D <- loadTexture2Base (w,h) nullPtr
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
	-- ~ glDepthFunc GL_LEQUAL
	glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (texId texD) 0

	bindfb $ Framebuffer 0
	return $ \r -> do
		bindfb fb
		glClear GL_COLOR_BUFFER_BIT
		display' r
		bindfb $ Framebuffer 0
		return texRGB
		-- untested and all



drawDepth :: Farbe m => m (Render m -> m (Texture D))
drawDepth = do
	(w',h') <- windowSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texD :: Texture D <- loadTexture2Base (w,h) nullPtr
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
	-- ~ glDepthFunc GL_LEQUAL
	glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (texId texD) 0
	bindfb $ Framebuffer 0
	return $ \r -> do
		bindfb fb
		glClear GL_DEPTH_BUFFER_BIT
		display' r
		bindfb $ Framebuffer 0
		return texD
		-- untested



renderTexture :: (MonadIO m, HandTex m, DelayedState SmallWorld m, ShaderCache (HandTexT IO) m)
	=> Var (Texture f) -> m ([VArray (V3 Float)] -> m ())
renderTexture t = compile $ \v -> do
	let V4 x y _ _ = fragCoord
	let V4 r g b a = (*0.5) $ texture (use t) $ V2 x (-y) * 0.001
	return (up 1 v, V4 r g b 1)



compile' :: (Farbe m, AttrType a b, DelayedState SmallWorld m, ShaderCache (HandTexT IO) m)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> Render m)
compile' a = fmap (DrawShader .) $ compile a

-- :: attribsandall => m Render


newtype Framebuffer = Framebuffer GLuint

genFramebuffer :: MonadIO m => m Framebuffer
genFramebuffer = liftIO $ fmap Framebuffer $ withPtr_ $ glGenFramebuffers 1

bindfb :: MonadIO m => Framebuffer -> m ()
bindfb (Framebuffer n) = glBindFramebuffer GL_FRAMEBUFFER n

framebufferStatus :: MonadIO m => m ()
framebufferStatus = do
	s <- glCheckFramebufferStatus GL_FRAMEBUFFER
	case s of
		GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> error "borked framebuffer attachment"
		GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS -> error "borked framebuffer dimensions"
		GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> error "missing attachments"
		GL_FRAMEBUFFER_UNSUPPORTED -> error "framebuffer setup unsupported"
		_ -> return ()

