
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Graphics.Farbe
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

  vstl <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray


  -- ~ f <- compile $ \((V3 a b c,V3 x y z)) -> do
    -- ~ let pos = V4 x y z 1
    -- ~ V2 x' y' <- transfer (V2 x (y + a * b * c * 0.00002))
    -- ~ return (pos, texture (use t) ((V2 1 (-0.5))*(V2 x' y')-0.5))

  f <- compile $ \((a, V3 x y z)) -> do
    let pos = V4 x y z 1
    V2 x' y' <- transfer (V2 x y)
    -- ~ a' <- transfer a
    -- ~ return (pos, up 1 a')
    return (pos, texture (use t) ((V2 1 (-0.5))*(V2 x' y')-0.5))


  -- ~ v <- newVArray $ frame

  fix $ \loop -> processEvents $ \es -> do
    glerrcheck
    f [vstl]
    display
    liftIO $ performGC
    loop





glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



