{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Graphics.Farbe
-- ~ import Graphics.Farbe.Window
import Graphics.Farbe.Shader

import Graphics.Farbe.Vec
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.OBJ

import Control.Monad

import Data.Maybe
import Data.Function



colorful'
	:: Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful' r v = let
	v' = r **| v
	n' = transferFrag v
	in (up 1 v', up 1 n' * 0.5 + 0.2)



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	-- ~ f <- compileShader colorful'
	fix $ \loop -> processEvents $ \es -> do
		va <- newVArray frame
		r <- rotationFromMouse33
		runShader colorful' r [va]
		-- ~ f r [va]
		loop

