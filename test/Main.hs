{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Shader
import Graphics.Farbe.Window
import Graphics.Farbe.Vec
import Graphics.Farbe.STL
import Graphics.Farbe.Texture
import Data.Function
import Data.Either
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Function

import Graphics.GL.Embedded20
import Graphics.GL.Types
import Graphics.Farbe
import Graphics.Farbe.Utils
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Window

import Foreign hiding (void)
import Codec.Picture


import Control.Monad.State.Strict

import Graphics.Farbe.GLFunc


import Graphics.GL.Types



import Debug.Trace
import System.Mem






main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do

	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
	r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

	f <- compile' $ \(n,v) -> do
		let v' = use r **| v
		n' <- transfer n
		return (up 1 v', up 1 n' * 0.5 + 0.2)

	-- ~ let g a = DrawShader $ f a

	fix $ \loop -> processEvents $ \es -> do
		glerrcheck
		case es of
			[(EventMouseMove (x,y),_)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
			_ -> return ()


		display $ f [cube, teapot]


		-- ~ liftIO $ performGC
		loop




glerrcheck :: MonadIO m => m ()
glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



