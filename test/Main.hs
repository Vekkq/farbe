{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Graphics.Farbe
-- ~ import Graphics.Farbe.Window
import Graphics.Farbe.Shader
import Graphics.Farbe.Uniform

import Graphics.Farbe.Vec
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.OBJ

import Control.Monad

import Data.Maybe
import Data.Function
import Data.Typeable



-- ~ frameShader :: Expr F Texture -> (V2 (Expr V Float)) -> (V4 (Expr V Float), V4 (Expr F Float))
-- ~ frameShader t (V2 x y) = (V4 x y 0.1 1, texture t (down fragCoord / 256))


frameShader' :: Var Texture -> (V2 (Expr V Float)) -> (V4 (Expr V Float), V4 (Expr F Float))
frameShader' t (V2 x y) | t' <- use t
	= (V4 x y 0.1 1, texture t' (down fragCoord / 256))


renderFrame :: (MonadWindow m, Farbe m, Typeable m) => m ()
renderFrame = do
	frame <- newVArray $ [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1), V2 1 (-1)]
	t <- loadImage "test-resources/fish_red1.jpg"
	fix $ \loop -> processEvents $ \es -> do
		runShaderV frameShader' t [frame]
		anyMouseClick es renderColorful
		loop

colorful
	:: Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful r v = let
	v' = r **| v
	n' = transferFrag v
	in (up 1 v', up 1 n' * 0.5 + 0.2)


renderColorful :: (MonadWindow m, Farbe m, Typeable m) => m ()
renderColorful = do
	va <- newVArray frame
	fix $ \loop -> processEvents $ \es -> do
		r <- rotationFromMouse33
		runShader colorful r [va]
		anyMouseClick es renderFrame
		loop



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	renderFrame

