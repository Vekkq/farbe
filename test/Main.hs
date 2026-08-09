{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Shader

import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.OBJ

import Control.Monad

import Data.Maybe
import Data.Function


colo :: HandShdr m => Mat V3 V3 Float -> VArray (V3 Float, V3 Float) -> m Bool
colo = composef colorful

colorful :: HandShdr m => m (Mat V3 V3 Float -> VArray (V3 Float, V3 Float) -> m Bool)
colorful = mkShader colorful'

colorful'
	:: Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float), V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful' r (n,v) = let
	v' = r **| v
	n' = transfer' n
	in (up 1 v', up 1 n' * 0.5 + 0.2)



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	-- ~ modifyConfig $ \f -> f { devDebugMode = True }
	undefined


