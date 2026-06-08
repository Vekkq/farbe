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
	, W.Display (..)
	-- * Event handling
	, processEvents
	, W.Event (..)
	, W.Key (..)
	, W.KeyState (..)
	-- * Shader definition
	, shader
	, ShaderDefi
	, isShaderCompiled
	, module Graphics.Farbe.Vec
	-- * Vertex array
	, VArray (..)
	, newVArray
	-- * Shader's Expr type
	, Expr
	, V
	, F
	, fragCoord
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
	) where


import Graphics.Farbe.State hiding (runFarbeT, runFarbeT')
import qualified Graphics.Farbe.Window as W
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


