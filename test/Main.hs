
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.Window
import Graphics.Farbe.Vec
import Graphics.Farbe.STL
import Data.Function
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Graphics.GL


data Texture

makeVarT :: MonadGL m => Float -> m (Var Texture)
makeVarT = makeVar

instance



main :: IO ()
main = runWindowT "" (InWindow (600,400)) $ runGL glDefaultConfig $ do



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











