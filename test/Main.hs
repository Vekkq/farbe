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

import Graphics.GL.Types



import Debug.Trace
import System.Mem






main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do

	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
	r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

	f <- compile $ \(n,v) -> do
		let v' = use r **| v
		n' <- transfer n
		return (up 1 v', up 1 n' * 0.5 + 0.2)
	glEnable GL_STENCIL_TEST

	fix $ \loop -> processEvents $ \es -> do
		glerrcheck
		case es of
			[(EventMouseMove (x,y),_)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
			_ -> return ()

		glEnable GL_STENCIL_TEST
		glClear GL_STENCIL_BUFFER_BIT
		glStencilOp GL_KEEP GL_DECR_WRAP GL_DECR_WRAP
		glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
		-- ~ glDepthMask GL_FALSE
		-- ~ glStencilMask 0xFF
		-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE
		f [cube]
		-- ~ glDepthMask GL_TRUE
		-- ~ glStencilMask GL_FALSE
		glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
		-- ~ glClear GL_DEPTH_BUFFER_BIT
		
		glStencilOp GL_KEEP GL_KEEP GL_KEEP
		glStencilFunc GL_LESS 1 0xFF
		-- ~ glStencilFunc GL_GREATER 331 1
		glDisable GL_DEPTH_TEST
		f [teapot]
		
		glStencilFunc GL_ALWAYS 0 0xFF
		glDisable GL_STENCIL_TEST
		
		-- ~ glStencilMask 0xFF
		-- ~ glStencilFunc GL_GREATER 1 1
		-- ~ glStencilFunc GL_GREATER 333 1
		-- ~ glClear GL_DEPTH_BUFFER_BIT
		-- ~ f [cube]
		
		t <- getTime

		display
		-- ~ liftIO $ performGC
		loop





glerrcheck :: MonadIO m => m ()
glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



