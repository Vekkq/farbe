{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe
	( runFarbeT
	, W.Display (..)
	, Config (..)
	, processEvents
	, module Graphics.Farbe.Vec
	, VArray (..)
	, newVArray
	, transfer
	, Var (..)
	, makeVar
	, use
	, swapVar
	, W.KeyState (..)
	, W.Event (..)
	, W.Key (..)
	, FarbeT
	, Farbe
	, AttrType
	-- * makeVar variants
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
	, Texture
	, loadTexture
	, makeVarT
	, MonadIO (..)
	, glErr
	, modifyConfig
	, shader
	, fragCoord
	, texture
	, drawOver
	, drawTexture
	, drawDepth
	, drawInto
	) where

import qualified Graphics.Farbe.State as S
import Graphics.Farbe.State hiding (runFarbeT)
import qualified Graphics.Farbe.Window as W
import Graphics.Farbe.Vec
import Graphics.Farbe.Uniform
import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Shader
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Vec ()
import Graphics.Farbe.Expr ()
import Graphics.Farbe.Utility
import Graphics.Farbe.GL ()
import Graphics.Farbe.Expr
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class ()
import Data.Maybe
import System.Mem

import Foreign.Ptr
import Data.Bits
import Graphics.GL
import Control.Concurrent.MVar



instance (Farbe m, Monad m) => Farbe (W.WindowT m) where
	stateFarbe = lift . stateFarbe

instance (ShaderEnv m, Monad m) => ShaderEnv (W.WindowT m) where
	stateShader = lift . stateShader


-- | The environment to do draw operations.

runFarbeT :: MonadIO m => String -> W.Display -> W.WindowT (S.FarbeT m) a -> m a
runFarbeT s d f = fmap fst . S.runFarbeT err . W.runWindowT s d $ do
	e <- emptyFarbeState
	putFarbe e
	devDebug "window creation passed."
	glClearColor 0.1 0.1 0.1 1
	glEnable GL_DEPTH_TEST
	glPixelStorei GL_UNPACK_ALIGNMENT 1
	f
	where
	 err = error "Farbe state not initialized yet"



processEvents :: (W.MonadWindow m, Farbe m)
	=> ([(W.Event, W.EventContext)] -> m ()) -> m ()
processEvents f = do
	runDelayed
	W.swapBuffers
	glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
	W.processEvents f

glerrcheck :: MonadIO m => m ()
glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e


runDelayed :: (W.MonadWindow m, Farbe m, MonadIO m) => m ()
runDelayed = do
	glerrcheck
	liftIO $ performGC
	work -- get at least one piece done per frame
	isEmpty <- join $ (liftIO . isEmptyMVar) <$> getsFarbe delayed
	tl <- getsFarbe lastFrameTime
	t <- W.getTime
	c <- getsConfig workTime
	if not isEmpty && t - tl > c
		then runDelayed
		else logTime
	where
		work :: (Farbe m, MonadIO m) => m ()
		work = do
			d <- getsFarbe delayed
			join $ fmap (liftFarbe . fromMaybe (return ())) $ liftIO $ tryTakeMVar d



drawOver :: MonadIO m => m a1 -> m a2 -> m ()
drawOver a b = do
	glEnable GL_STENCIL_TEST
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_DECR_WRAP
	a
	glStencilFunc GL_GREATER 1 1
	b
	glDisable GL_STENCIL_TEST


drawInto :: MonadIO m => m a1 -> m a2 -> m ()
drawInto a b = do
	glEnable GL_STENCIL_TEST
	glClear GL_STENCIL_BUFFER_BIT
	glStencilOp GL_KEEP GL_DECR_WRAP GL_DECR_WRAP
	glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
	a
	glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE

	glStencilOp GL_KEEP GL_KEEP GL_KEEP
	glStencilFunc GL_LESS 1 0xFF
	glDisable GL_DEPTH_TEST
	b

	glStencilFunc GL_ALWAYS 0 0xFF
	glDisable GL_STENCIL_TEST





drawTexture :: (Monad m, Farbe m, W.MonadWindow m) => m (m () -> m Texture)
drawTexture = do
	(w',h') <- W.windowSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texRGB <- newTexture RGB (V2 w h) nullPtr
	idRGB <- getTexId texRGB
	glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D idRGB 0
	-- replace texture with renderbuffer in this function
	texD <- newTexture D (V2 w h) nullPtr
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
	-- ~ glDepthFunc GL_LEQUAL
	idD <- getTexId texD
	glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D idD 0

	bindfb $ Framebuffer 0
	return $ \r -> do
		bindfb fb
		glClear GL_COLOR_BUFFER_BIT
		r
		bindfb $ Framebuffer 0
		return texRGB
		-- untested and all



drawDepth :: (Monad m, Farbe m, W.MonadWindow m) => m (m () -> m Texture)
drawDepth = do
	(w',h') <- W.windowSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texD <- newTexture D (V2 w h) nullPtr
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
	-- ~ glDepthFunc GL_LEQUAL
	idD <- getTexId texD
	glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D idD 0
	bindfb $ Framebuffer 0
	return $ \r -> do
		bindfb fb
		glClear GL_DEPTH_BUFFER_BIT
		r
		bindfb $ Framebuffer 0
		return texD
		-- untested



-- ~ renderTexture :: (MonadIO m, HandTex m, DelayedState SmallWorld m, ShaderCache (HandTexT IO) m)
	-- ~ => Var (Texture f) -> m ([VArray (V3 Float)] -> m ())
-- ~ renderTexture t = compile $ \v -> do
	-- ~ let V4 x y _ _ = fragCoord
	-- ~ let V4 r g b a = (*0.5) $ texture (use t) $ V2 x (-y) * 0.001
	-- ~ return (up 1 v, V4 r g b 1)




-- ~ compile' :: (Farbe m, AttrType a b, DelayedState SmallWorld m, ShaderCache (HandTexT IO) m)
	-- ~ => (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-- ~ -> m ([VArray a] -> Render m)
-- ~ compile' a = fmap (DrawShader .) $ compile a

-- :: attribsandall => m Render


newtype Framebuffer = Framebuffer GLuint

genFramebuffer :: MonadIO m => m Framebuffer
genFramebuffer = liftIO $ fmap Framebuffer $ withPtr_ $ glGenFramebuffers 1

bindfb :: (MonadIO m) => Framebuffer -> m ()
bindfb (Framebuffer n) = glBindFramebuffer GL_FRAMEBUFFER n

-- ~ framebufferStatus :: (MonadIO m) => m ()
-- ~ framebufferStatus = do
	-- ~ s <- glCheckFramebufferStatus GL_FRAMEBUFFER
	-- ~ case s of
		-- ~ GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> error "borked framebuffer attachment"
		-- ~ GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS -> error "borked framebuffer dimensions"
		-- ~ GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> error "missing attachments"
		-- ~ GL_FRAMEBUFFER_UNSUPPORTED -> error "framebuffer setup unsupported"
		-- ~ _ -> return ()


