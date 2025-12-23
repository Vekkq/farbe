
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Shader
import Graphics.Farbe.Window
import Graphics.Farbe.Vec
import Graphics.Farbe.STL
import Graphics.Farbe.Texture
import Data.Function
import Data.Either
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Graphics.GL.Embedded20
import Graphics.Farbe.Utils
import Graphics.Farbe.JuicyPixels

import Debug.Trace
import System.Mem



main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do
  i <- loadImage' RGB "test-resources/iwi.jpg"
  t <- makeVarT i

  i2 <- loadImage' RGB "test-resources/ayataka512.jpg"
  t2 <- makeVarT i2

  teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
  cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
  eiffel <- readFileBinSTL "test-resources/eiffel.stl" >>= newVArray
  -- ~ cstl <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
  -- ~ cstl2 <- readFileSTL "test-resources/cube.stl" >>= newVArray
  -- ~ vstl <- readFileSTL "test-resources/cube.stl" >>= newVArray . map (0.9*|)

  r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

  f <- compile $ \(n,v) -> do
    let v' = use r **| v
    n' <- transfer n
    return (up 1 v', up 1 n' * 0.5 + 0.2)

  -- ~ g <- compile $ \(v) -> do
    -- ~ let (V3 x y z) = use r **| v * 0.2
    -- ~ vt <- transfer (V2 x (-y))
    -- ~ return (V4 x y z 1, pure 0.9)


  -- ~ v <- newVArray $ frame

  fix $ \loop -> processEvents $ \es -> do
    -- ~ glClear GL_DEPTH_BUFFER_BIT
    glerrcheck
    r' <- readVar r
    case es of
      [(EventMouseMove (x,y),_)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
      _ -> return ()

    stencil GL_EQUAL 1 1 GL_REPLACE GL_KEEP GL_KEEP [f [teapot]] $ f [eiffel]
    -- ~ f [cube]
    -- ~ f [teapot]
    -- ~ f [eiffel]
    -- ~ f [eiffel, teapot, cube]

    display
    liftIO $ performGC
    loop





glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



