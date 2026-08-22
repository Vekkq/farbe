{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Window
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


colorful''
	:: (V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful'' v = let
	v' = v
	n' = transferFrag v
	in (up 1 v', up 1 n' * 0.5 + 0.2)



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	-- ~ modifyConfig $ \f -> f { devDebugMode = True }
	-- ~ fix $ \loop -> do
		va <- newVArray frame
		glErr
		runShader colorful'' [va]
		runShader colorful' identity [va]
		return ()

		-- ~ loop


