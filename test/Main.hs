{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.OBJ

import Control.Monad

import Data.Maybe
import Data.Function


basicShader :: Farbe m => Var Texture -> Var (Mat V3 V3 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
basicShader t r = shader $ \(n,v) -> do
	let v' = use r **| v
	n' <- transfer n
	let c = down fragCoord
	return (up 1 v', up 1 n' * 0.5 + 0.5*texture (use t) (c / 256) + textureIO "test-resources/ayataka512.jpg" (c / 256 + 0.5))


renderbasic r = do
	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray

	t <- makeVarT =<< loadImage "test-resources/KorDrTtaa42.png"

	fix $ \loop -> processEvents $ \es -> do
		updateRotate es r
		basicShader t r [cube, teapot]
		anyMouseClick es $ renderobj r
		loop



shaderobj :: Farbe m => Var (Mat V3 V3 Float) -> [VArray OBJPoint] -> m ()
shaderobj r = shader $ \(OBJPointE v n t) -> do
	let v' = use r **| v
	t' <- transfer $ down t
	return (up 1 v', textureIO "test-resources/fish_red.jpg" t')


renderobj r = do
	fishv <- loadOBJ "test-resources/fish_red.obj"
	fish <- newVArray $ map (\op -> op { oCoord = 0.1 * oCoord op }) fishv
	fix $ \loop -> processEvents $ \es -> do
		shaderobj r [fish]
		updateRotate es r
		anyMouseClick es $ renderbasic r
		loop

anyMouseClick es f = case es of
	[(EventMouseKey _ _ Down, _)] -> f
	_ -> return ()


updateRotate es r = case es of
	[(EventMouseMove (x,y), _)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
	_ -> return ()




main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)
	renderobj r
	-- ~ renderbasic r

	-- ~ modifyConfig $ \f -> f { devDebugMode = True }

