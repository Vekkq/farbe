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



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	renderColorful



frameShader
	:: Expr F Texture -- | single texture parameter
	-> (V2 (Expr V Float)) -- | vertex array attribute
	-> (V4 (Expr V Float), V4 (Expr F Float)) -- | shader result tuple - alias SResult
frameShader t (V2 x y) = (V4 x y 0.1 1, texture t (down fragCoord / 256))


renderFrame :: (MonadWindow m, Farbe m) => m ()
renderFrame = do
	frame <- newVArray $ [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1), V2 1 (-1)]
	t <- loadImage "test-resources/fish_red.jpg"
	fix $ \loop -> processEvents $ \es -> do
		runShader frameShader t [frame]
		anyMouseClick es renderobj
		loop

colorful
	:: Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
colorful r v = let
	v' = r **| v
	n' = transferFrag v
	in (up 1 v', up 1 n' * 0.5 + 0.2)


renderColorful :: (MonadWindow m, Farbe m) => m ()
renderColorful = do
	va <- newVArray frame
	fix $ \loop -> processEvents $ \es -> do
		r <- rotationFromMouse33
		runShader colorful r [va]
		anyMouseClick es renderFrame
		loop


shaderobj
	:: Expr F Texture -> Mat V3 V3 (Expr V Float)
	-> (V3 (Expr V Float), V3 (Expr V Float), V3 (Expr V Float))
	-> SResult
shaderobj tex r (v, t, n) = let
		v' = r **| v
		t' = transferFrag $ down t
	in (up 1 v', texture tex t')

renderobj :: (MonadWindow m, Farbe m) => m ()
renderobj = do
	t <- loadImage "test-resources/fish_red.jpg"
	fishv <- loadOBJ "test-resources/fish_red.obj"
	fish <- newVArray $ map (\(OBJPoint v t n) -> (0.1 * v, t, n)) fishv
	fix $ \loop -> processEvents $ \es -> do
		r <- rotationFromMouse33
		runShader shaderobj t r [fish]
		anyMouseClick es $ renderColorful
		loop

