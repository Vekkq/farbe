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
	, F
	, fragCoord
	, windowDim
	, windowDim0to1
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
import Control.Monad.Trans
import Control.Monad

import qualified Graphics.UI.GLFW as W


updateRotate es r = case es of
	[(EventMouseMove (x,y), _)] -> void $ swapVar r $ rotationMatrix 0 (-y*0.01) (-x*0.01)
	_ -> return ()


windowDim :: (MonadWindow m) => m (V2 (Expr e Float))
windowDim = do
	w <- glfwWindow
	return $ livingExprV "dim" $ do
		(x,y) <- liftIO $ W.getFramebufferSize w
		return $ fromIntegral <$> V2 x y


windowDim0to1 :: (MonadWindow m) => m (V2 (Expr F Float))
windowDim0to1 = do
	e <- windowDim
	return $ down fragCoord / e

