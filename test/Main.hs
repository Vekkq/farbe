{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Window
import Graphics.Farbe.Vec
import Graphics.Farbe.STL

import Control.Monad

import Data.Function


main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do

	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
	r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

	f <- compile $ \(n,v) -> do
		let v' = use r **| v
		n' <- transfer n
		return (up 1 v', up 1 n' * 0.5 + 0.2)

	-- ~ let g a = DrawShader $ f a

	fix $ \loop -> processEvents $ \es -> do
		swapBuffers
		glerrcheck
		case es of
			[(EventMouseMove (x,y), _)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
			_ -> return ()



		f [cube, teapot]


		-- ~ liftIO $ performGC
		case es of
			[(EventKey Key'Escape Down _, _)] -> return ()
			_ -> loop


