{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main (main) where

import Graphics.Farbe
-- ~ import Graphics.Farbe.Window
-- ~ import Graphics.Farbe.Vec (V3, Mat, (**|))
import Graphics.Farbe.Vec
-- ~ import Graphics.Farbe.GL
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.Texture
import Graphics.Farbe.Expr
import Graphics.Farbe.BuildShader

import Control.Monad

import Data.Maybe
import Data.Function




colorful :: Farbe m => Var Texture -> Var (Mat V3 V3 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
colorful t r = shader $ \(n,v) -> do
	let v' = use r **| v
	n' <- transfer n
	-- ~ let n'' = down n'
	let c = down fragCoord
	-- ~ return (up 1 v', up 1 n' * 0.5 + textureIO "test-resources/KorDrTtaa42.png" (c / 256))
	return (up 1 v', up 1 n' * 0.5 + texture (use t) (c / 256))
	--
	-- ~ return (up 1 v', up 1 (n' * 0.5 + textureIO (down n') "test-resources/KorDrTtaa4.png"))



main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	-- ~ modifyConfig $ \f -> f { devDebugMode = True }

	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
	r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

	t <- makeVarT =<< loadImage "test-resources/KorDrTtaa42.png"


	fix $ \loop -> processEvents $ \es -> do

		case es of
			[(EventMouseMove (x,y), _)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
			_ -> return ()

		colorful t r [cube, teapot]

		-- ~ liftIO $ performGC
		case es of
			[(EventKey Key'Escape Down _, _)] -> return ()
			_ -> loop


