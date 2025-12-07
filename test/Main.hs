
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
-- ~ import Graphics.GL

import Graphics.GL.Embedded20
import Graphics.Farbe.Utils
import Graphics.Farbe.JuicyPixels


main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbe $ do
  i <- loadImage' RGB "test-resources/iwi.jpg"
  t <- makeVarT i

  i2 <- loadImage' RGB "test-resources/ayataka512.jpg"
  t2 <- makeVarT i2


  f <- compile $ \(V3 x y z) -> do
    let pos = V4 x y z 1
    V2 x' y' <- transfer (V2 x y)
    return (pos, texture (use t) ((V2 1 (-0.5))*(V2 x' y')-0.5))
  -- ~ liftIO $ print =<< texId <$> readVar' t
  v <- frame

  -- ~ liftIO $ withPtr_ (glGetIntegerv GL_MAX_TEXTURE_SIZE) >>= print

  fix $ \loop -> processEvents $ \es -> do
    liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e
    -- ~ putVar t i
    -- ~ putVar t2 i2
    f [v]
    display
    loop



-- ~ main :: IO ()
-- ~ main = runWindowT "" (InWindow (600,400)) $ runGL glDefaultConfig $ do

  -- ~ a <- loadSTL "test/teapot.stl"
  -- ~ b <- loadSTL "test/cube.stl"
  -- ~ u <- makeVar =<< (newArr [0.1, 0.2..] :: MonadIO m => m (Arr 10 Float))
  -- ~ i <- makeVarI 1

  -- ~ f <- compile $ \v -> do
    -- ~ let (V3 x y z) = v*0.02
    -- ~ let pos = V4 x y z 1
    -- ~ x' <- transfer x
    -- ~ return (pos, V4 (use u `arr'` use i) x' 1 1)

  -- ~ g <- compile $ \v -> do
    -- ~ let (V3 x y z) = v*0.04
    -- ~ let pos = V4 x y z 1
    -- ~ x' <- transfer x
    -- ~ return (pos, V4 1 x' 1 1)

  -- ~ fix $ \loop -> processEvents $ \es -> do
    -- ~ liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e
    -- ~ t <- getTime
    -- ~ putVar i $ mod (floor t) 8
    -- ~ g [b]
    -- ~ f [a,b]
    -- ~ display
    -- ~ loop











