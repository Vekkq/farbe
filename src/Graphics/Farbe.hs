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
	, Display (..)
	-- * Event handling
	, processEvents
	, Event (..)
	, Key (..)
	, KeyState (..)
	-- * Shader definition
	-- ~ , isShaderCompiled
	, module Graphics.Farbe.Vec
	-- * Vertex array
	, VArray (..)
	, newVArray
	, frame
	, floorFrame
	-- * Shader's Expr type
	, Expr
	, V
	, viewMat
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
	, transfer
	-- * Make mutable shared variables for shaders

	, Texture
	, texture
	, texture'
	, loadTexture
	, Attribute
	-- * Rendering control
	, drawOver
	, drawTexture
	, drawDepth
	, drawInto
	-- * Configuration options
	, modifyConfig
	, Config (..)
	, MonadIO (..)
	-- * Miscellaneous
	, FarbeT
	, Farbe
	, runFarbeT'
	, MonadWindow
	, glErr
	) where


import qualified Graphics.Farbe.Farbe as S
import Graphics.Farbe.Farbe hiding (runFarbeT, runFarbeT')
import qualified Graphics.Farbe.Window as W
import Graphics.Farbe.Window hiding (processEvents)
import Graphics.Farbe.Expr
import Graphics.Farbe.Shader
import Graphics.Farbe.Vec
import Graphics.Farbe.Texture
import Graphics.Farbe.Utility
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Attribute
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class ()
import Data.Maybe
import Data.Set (member)
import System.Mem
import Graphics.Farbe.Params

import Foreign.Ptr
import Data.Bits
import Graphics.GL
import Control.Concurrent
-- ~ import Control.Concurrent.MVar

import qualified Graphics.Farbe.Shader as S

class GLWindow a

instance (Farbe m, Monad m) => Farbe (W.WindowT m) where
	stateFarbe = lift . stateFarbe



-- | The environment to do draw operations.
--   It spawns a window with the render context.
runFarbeT :: MonadIO m => String -> W.Display -> S.FarbeT (W.WindowT m) a -> m a
runFarbeT s d = W.runWindowT s d . runFarbeT'


runFarbeT' :: MonadIO m => S.FarbeT m a -> m a
runFarbeT' f = fmap fst . S.runFarbeT $ do
	glClearColor 0.1 0.1 0.1 1
	glEnable GL_DEPTH_TEST
	glPixelStorei GL_UNPACK_ALIGNMENT 1
	a <- f
	-- ~ liftIO $ yield
	-- ~ runDelayed
	return a


-- | @processEvents@ obtains the events and sends it to a provided function. The function is called, when the program isn't asked to quit. This function also controls the render pipeline (swap buffers).
processEvents :: (W.MonadWindow m, Farbe m)
	=> ([(W.Event, W.EventContext)] -> m ()) -> m ()
processEvents f = do
	es <- processEvents'
	b <- shouldWindowClose
	if b || isEsc es || isAltF4 es
	then return ()
	else f es


isEsc :: [(Event, b)] -> Bool
isEsc es = case es of
	[(EventKey Key'Escape Down _, _)] -> True
	_ -> False


isAltF4 :: [(W.Event, W.EventContext)] -> Bool
isAltF4 es = case es of
	[(EventKey Key'F4 Down _, c)] | member (Right Key'LeftAlt) c -> True
	_ -> False

processEvents' :: (MonadWindow m, Farbe m) => m [(W.Event, W.EventContext)]
processEvents' = do
	runDelayed
	W.swapBuffers
	glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
	W.processEvents


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
		-- ~ work :: (Farbe m, MonadIO m) => m ()
		-- ~ work = do
			-- ~ d <- getsFarbe delayed
			-- ~ join $ fmap (applyFarbe . fromMaybe (return ())) $ liftIO $ tryTakeMVar d
		work = undefined


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



-- ~ updateRotate' :: MonadIO m
	-- ~ => [(Event, b)] -> Var (Mat V3 V3 Float) -> m (Mat V3 V3 Float)
-- ~ updateRotate' es r = case es of
	-- ~ [(EventMouseMove (x,y), _)] -> do
		-- ~ let m = rotationMatrix 0 (-y*0.01) (-x*0.01)
		-- ~ swapVar r m
		-- ~ return m
	-- ~ _ -> readVar r

-- ~ updateRotate :: MonadWindow m => Var (Mat V3 V3 Float) -> m ()
-- ~ updateRotate r = do
	-- ~ V2 x y <- lastCoord
	-- ~ let m = rotationMatrix 0 (-y*0.01) (-x*0.01)
	-- ~ void $ swapVar r m


viewMat :: MonadWindow m => m (Mat V4 V4 Float)
viewMat = do
	V2 x y <- lastCoord
	return $ perspective 1 1 0.01 100 **** translateM (V3 0 0 (-3)) **** rotationMatrix4 0 (y*0.01) (-x*0.01)

-- ~ perspectiveMatrix :: MonadWindow m => Float -> m (Mat V4 V4 Float)
-- ~ perspectiveMatrix fov = do
	-- ~ V2 x y <- windowDim
	-- ~ return $ inversePerspective fov (x/y) (-1) 1

-- ~ inversePerspective
  -- ~ :: Floating a
  -- ~ => a -- ^ FOV (y direction, in radians)
  -- ~ -> a -- ^ Aspect ratio
  -- ~ -> a -- ^ Near plane
  -- ~ -> a -- ^ Far plane
  -- ~ -> Mat V4 V4 a
-- ~ inversePerspective fovy aspect near far =
  -- ~ V4 (V4 a 0 0 0   )
     -- ~ (V4 0 b 0 0   )
     -- ~ (V4 0 0 0 (-1))
     -- ~ (V4 0 0 c d   )
  -- ~ where tanHalfFovy = tan $ fovy / 2
        -- ~ a = aspect * tanHalfFovy
        -- ~ b = tanHalfFovy
        -- ~ c = oon - oof
        -- ~ d = oon + oof
        -- ~ oon = 0.5/near
        -- ~ oof = 0.5/far



windowDim :: MonadWindow m => m (V2 Float)
windowDim = do
	w <- glfwWindow
	(x,y) <- liftIO $ W.getFramebufferSize w
	return $ fromIntegral <$> V2 x y


-- ~ windowDim :: MonadWindow m => m (V2 (Expr e Float))
-- ~ windowDim = do
	-- ~ w <- glfwWindow
	-- ~ return $ livingExprV "dim" $ do
		-- ~ (x,y) <- liftIO $ W.getFramebufferSize w
		-- ~ return $ fromIntegral <$> V2 x y


-- ~ windowDim0to1 :: (MonadWindow m) => m (V2 (Expr F Float))
-- ~ windowDim0to1 = do
	-- ~ e <- windowDim
	-- ~ return $ down fragCoord / e

