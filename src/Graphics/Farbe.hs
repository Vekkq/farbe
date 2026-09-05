{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

-- | A OpenGL ES 2 rendering library.
--
-- GLES2 is ancient, but good enough for most things~
--
-- A sample program:
--
-- @
-- import Graphics.Farbe
-- import Data.Function (fix)
--
-- main :: IO ()
-- main = runFarbeT "Window title" (InWindow (1000,800)) $ renderColorful
--
-- renderColorful :: (MonadWindow m, Farbe m) => m ()
-- renderColorful = do
--   va <- newVArray frame
--   fix $ \loop -> processEvents $ \es -> do
--     r <- rotationFromMouse33
--     runShader colorful r [va]
--     loop
--
-- colorful
--   :: Mat V3 V3 (Expr V Float)               -- rotation matrix parameter
--   -> (V3 (Expr V Float))                    -- vertex array attribute parameter
--   -> (V4 (Expr V Float), V4 (Expr F Float)) -- shader result
-- colorful r v = let
--   v' = r **| v
--   n' = transferFrag v
--   in (up 1 v', up 1 n' * 0.5 + 0.2)
--
-- @

module Graphics.Farbe
	-- * Farbe
	( runFarbeT
	, Display (..)
	-- ** Event handling
	, processEvents
	, Event (..)
	, Key (..)
	, KeyState (..)
	, anyMouseClick
	, rotationFromMouse33
	, viewMat
	-- ** Shader definition
	-- | Example:
	--
	-- @
	-- foo :: Farbe m => V3 (V3 Float) -> [VArray (V3 Float)] -> m Bool
	-- foo = runShader colorful
	--
	-- colorful :: Mat V3 V3 (Expr V Float) -> (V3 (Expr V Float)) -> (V4 (Expr V Float), V4 (Expr F Float))
	-- colorful = undefined
	-- @
	--
	-- A shader function is defined by
	--
	-- * a number of Uniform parameters
	--
	-- * one Attribute parameter
	--
	-- * a result
	--
	-- Uniforms are defined by the `Use` class and convert values as follows:
	--
	-- +-------------------------+----------------------------+
	-- | Real space              | Shader space               |
	-- +=========================+============================+
	-- | @Float@                 | @(Expr e Float)@           |
	-- +-------------------------+----------------------------+
	-- | @Int32@                 | @(Expr e Int32)@           |
	-- +-------------------------+----------------------------+
	-- | @Bool@                  | @(Expr e Bool)@            |
	-- +-------------------------+----------------------------+
	-- | @Texture@               | @(Expr e Texture)@         |
	-- +-------------------------+----------------------------+
	-- | @(V2 Float)@            | @(V2 (Expr e Float))@      |
	-- +-------------------------+----------------------------+
	-- | @(V2 Int32)@            | @(V2 (Expr e Int32))@      |
	-- +-------------------------+----------------------------+
	-- | @(V2 Bool)@             | @(V2 (Expr e Bool))@       |
	-- +-------------------------+----------------------------+
	-- | @(V3 Float)@            | @(V3 (Expr e Float))@      |
	-- +-------------------------+----------------------------+
	-- | @(V3 Int32)@            | @(V3 (Expr e Int32))@      |
	-- +-------------------------+----------------------------+
	-- | @(V3 Bool)@             | @(V3 (Expr e Bool))@       |
	-- +-------------------------+----------------------------+
	-- | @(V4 Float)@            | @(V4 (Expr e Float))@      |
	-- +-------------------------+----------------------------+
	-- | @(V4 Int32)@            | @(V4 (Expr e Int32))@      |
	-- +-------------------------+----------------------------+
	-- | @(V4 Bool)@             | @(V4 (Expr e Bool))@       |
	-- +-------------------------+----------------------------+
	-- | @(V2 (V2 Float))@       | @(V2 (V2 (Expr e Float)))@ |
	-- +-------------------------+----------------------------+
	-- | @(V3 (V3 Float))@       | @(V3 (V3 (Expr e Float)))@ |
	-- +-------------------------+----------------------------+
	-- | @(V4 (V4 Float))@       | @(V4 (V4 (Expr e Float)))@ |
	-- +-------------------------+----------------------------+
	--
	-- Attributes are defined by `Attribute` in a scheme similar to Uniforms.
	--
	-- The result of a shader is a 2-tuple of:
	--
	-- * @(V4 (Expr V Float))@ determines the position of a vertex of a triangle in a
	--   render of coordinates -1 to 1 on x, y and z . w as homogeneous coordinate.
	--
	-- * @(V4 (Expr F Float))@ determines the color in rgb for every rendered pixel.
	, SResult
	, runShader
	, runShaderV
	-- | "Graphics.Farbe.Vec" is the default math module for Farbe.
	, module Graphics.Farbe.Vec
	, Expr
	, V
	, F
	, fragCoord
	, windowDim
	, napier
	, ln
	, modf
	, equot
	, erem
	, ediv
	, emod
	, transferFrag
	, texture
	-- ** Vertex array
	, VArray
	, newVArray
	, Attribute
	, frame
	, floorFrame
	-- ** Textures on shaders
	, Texture
	, loadImage
	, loadImagePixellated
	-- ** Render control
	-- | These functions are for combining multiple renders, using mostly stencils internally.
	--
	-- WIP likely broken functions.
	, drawOver
	, drawTexture
	, drawDepth
	, drawInto
	-- ** Configuration options
	, modifyConfig
	, Config (..)
	-- ** Miscellaneous
	, Use
	, FarbeT
	, Farbe
	, runFarbeT'
	, MonadWindow
	, MonadIO (..)
	-- ~ , glErr
	) where


import qualified Graphics.Farbe.Farbe as S
import Graphics.Farbe.Farbe hiding (runFarbeT, runFarbeT')
import qualified Graphics.Farbe.Window as W
import Graphics.Farbe.Window hiding (processEvents)
import Graphics.Farbe.Expr
import Graphics.Farbe.Vec
import Graphics.Farbe.Texture
import Graphics.Farbe.Utility
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Attribute
import Graphics.Farbe.Shader
import Graphics.Farbe.Uniform
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class ()
import Data.Maybe
import Data.Set (member)
import System.Mem

import Foreign.Ptr
import Data.Bits
import Graphics.GL
import Control.Concurrent

import Data.Typeable

import Graphics.Farbe.JuicyPixels



-- | The environment to do draw operations.
--   It spawns a window with the render context.
runFarbeT :: (MonadIO m, Typeable m) => String -> W.Display -> S.FarbeT (W.WindowT m) a -> m a
runFarbeT s d = W.runWindowT s d . runFarbeT'


runFarbeT' :: (MonadIO m, Typeable m) => S.FarbeT m a -> m a
runFarbeT' f = fmap fst . S.runFarbeT $ do
	(V4 r g b a) <- getsConfig backgroundColor
	glClearColor r g b a
	glEnable GL_DEPTH_TEST
	glPixelStorei GL_UNPACK_ALIGNMENT 1
	glPixelStorei GL_PACK_ALIGNMENT 1 -- for textures coming from gpu
	r' <- f
	-- ~ liftIO $ yield
	-- ~ runDelayed
	return r'


-- | @processEvents@ Sends window events to a provided function. The function is always called, as long as the program isn't asked to quit. This function also controls the render pipeline - swapping buffers, clear screen, etc.
processEvents :: (W.MonadWindow m, Farbe m)
	=> ([(W.Event, W.EventContext)] -> m ()) -> m ()
processEvents f = do
	es <- processEvents'
	b <- shouldWindowClose
	if b || isEsc es || isAltF4 es
	then return ()
	else f es

processEvents' :: (MonadWindow m, Farbe m) => m [(W.Event, W.EventContext)]
processEvents' = do
	runDelayed
	W.swapBuffers
	glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
	W.processEvents

isEsc :: [(Event, b)] -> Bool
isEsc es = case es of
	[(EventKey Key'Escape Down _, _)] -> True
	_ -> False

isAltF4 :: [(W.Event, W.EventContext)] -> Bool
isAltF4 es = case es of
	[(EventKey Key'F4 Down _, c)] | member (Right Key'LeftAlt) c -> True
	_ -> False

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


anyMouseClick :: Monad m => [(W.Event, W.EventContext)] -> m () -> m ()
anyMouseClick es f = case es of
	[(EventMouseKey _ _ Down, _)] -> f
	_ -> return ()

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
	(w',h') <- W.fbSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texRGB <- newTexture defaultRGB (V2 w h) nullPtr
	idRGB <- getTexId texRGB
	glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D idRGB 0
	-- replace texture with renderbuffer in this function
	texD <- newTexture defaultD (V2 w h) nullPtr
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
	(w',h') <- W.fbSize
	let (w,h) = (itoi w', itoi h')
	fb <- genFramebuffer
	bindfb fb
	texD <- newTexture defaultD (V2 w h) nullPtr
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


rotationFromMouse33 :: MonadWindow m => m (Mat V3 V3 Float)
rotationFromMouse33 = do
	V2 x y <- lastCoord
	return $ rotationMatrix 0 (-y*0.01) (-x*0.01)


viewMat :: MonadWindow m => m (Mat V4 V4 Float)
viewMat = do
	V2 x y <- lastCoord
	return $ perspective 1 1 0.01 100 **** translateM (V3 0 0 (-3)) **** rotationMatrix4 0 (y*0.01) (-x*0.01)

-- Window dimensions.
windowDim :: MonadWindow m => m (V2 Float)
windowDim = do
	w <- glfwWindow
	(x,y) <- liftIO $ W.getFramebufferSize w
	return $ fromIntegral <$> V2 x y

