{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-|
Module      : Graphics.Farbe
Copyright   : (c) vekkq, 2026
License     : BSD-3-Clause
Maintainer  : vekkq@vivaldi.net
Stability   : experimental

This library abstracts away traps of OpenGL and provides its basics for rendering.
-}
module Graphics.Farbe
	( runFarbeT
	, Display (..)
	-- * Event handling
	, processEvents
	, Event (..)
	, Key (..)
	, KeyState (..)
	-- * Shader definition
	, shader
	, ShaderDefi
	, isShaderCompiled
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
	, use
	-- * Make mutable shared variables for shaders
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
	, updateRotate
	, Texture
	, texture
	, texture'
	, loadTexture
	, Var (..)
	, swapVar
	, AttrType
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
	) where


import Graphics.Farbe.State hiding (runFarbeT, runFarbeT')
import Graphics.Farbe.Window hiding (processEvents)
import Graphics.Farbe.Farbe
import Graphics.Farbe.Vec
import Graphics.Farbe.Uniform
import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Shader
import Graphics.Farbe.BuildShader
import Graphics.Farbe.Expr
import Graphics.Farbe.Params
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent

import qualified Graphics.UI.GLFW as W


updateRotate' :: MonadIO m
	=> [(Event, b)] -> Var (Mat V3 V3 Float) -> m (Mat V3 V3 Float)
updateRotate' es r = case es of
	[(EventMouseMove (x,y), _)] -> do
		let m = rotationMatrix 0 (-y*0.01) (-x*0.01)
		swapVar r m
		return m
	_ -> readVar r

updateRotate :: MonadWindow m => Var (Mat V3 V3 Float) -> m ()
updateRotate r = do
	V2 x y <- lastCoord
	let m = rotationMatrix 0 (-y*0.01) (-x*0.01)
	void $ swapVar r m


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

