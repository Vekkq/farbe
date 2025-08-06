module Main (main) where

import STL
import Graphics.Farbe
import Graphics.Farbe.Window
import Graphics.Farbe.Vec
import Data.Function
import Control.Monad
import Control.Monad.IO.Class
import Graphics.GL




loadSTL :: MonadGL m => String -> m (GArray (V3 Float))
loadSTL s = readFileSTL s >>= newGArray


main :: IO ()
main = foo

foo :: IO ()
foo = runWindowT "" (InWindow (600,400)) $ runGL $ do
  a <- loadSTL "test/teapot.stl"
  b <- loadSTL "test/cube.stl"
  (u, upx) <- makeFloat

  f <- compile (\v -> let (V3 x y z) = v*0.02 in V4 u 1 (raster (V4 x y z 1, x)) 1)
  g <- compile (\v -> let (V3 x y z) = v*0.04 in V4 1 (raster (V4 x y z 1, x)) 1 1)

  fix $ \loop -> processEvents $ \es -> do
    liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e
    g [b]
    f [a,b]
    display
    loop











