
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


main = do
  main1
  performGC

main1 :: IO ()
main1 = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do
  i <- loadImage' RGB "test-resources/iwi.jpg"
  t <- makeVarT i

  i2 <- loadImage' RGB "test-resources/ayataka512.jpg"
  t2 <- makeVarT i2


  f <- compile $ \(V3 x y z) -> do
    let pos = V4 x y z 1
    V2 x' y' <- transfer (V2 x y)
    return (pos, texture (use t) ((V2 1 (-0.5))*(V2 x' y')-0.5))
  v <- newVArray $ frame

  foo f

  fix $ \loop -> processEvents $ \es -> do
    glerrcheck
    f [v]
    display
    i <- getTime
    liftIO $ print i
    liftIO $ performGC
    loop



foo f = do
  v <- newVArray $ map (+V3 0.1 0 0) frame

  fix $ \loop -> processEvents $ \es -> do
    f [v]
    display
    i <- getTime
    liftIO $ print i
    when (i < 1) loop



glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



