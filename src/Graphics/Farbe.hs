{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe where

import Graphics.Farbe.Window
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Shader
import Graphics.Farbe.State

import Data.Map
import Data.Dynamic
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

import System.Mem.StableName
import Control.Exception
import Control.Concurrent.MVar

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS

import Control.Monad.IO.Class


{-
nextFrame :: (DelayedState m m, FrameTiming m, MonadIO m, MonadConfig m, MonadWindow m) => m ()
nextFrame = do
	doDelayedWork
	tl <- frameTimeGet
	t <- getTime
	c <- config
	if t - tl > workTime c
		then doDelayedWork
		else do
			logTime
			swapBuffers

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

-}
