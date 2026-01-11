
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




newtype Framebuffer = Framebuffer GLuint

genFramebuffer :: MonadIO m => m Framebuffer
genFramebuffer = liftIO $ fmap Framebuffer $ withPtr_ $ glGenFramebuffers 1

bindfb :: MonadIO m => Framebuffer -> m ()
bindfb (Framebuffer n) = glBindFramebuffer GL_FRAMEBUFFER n

framebufferStatus :: MonadIO m => m ()
framebufferStatus = do
  s <- glCheckFramebufferStatus GL_FRAMEBUFFER
  case s of
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> error "borked framebuffer attachment"
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS -> error "borked framebuffer dimensions"
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> error "missing attachments"
    GL_FRAMEBUFFER_UNSUPPORTED -> error "framebuffer setup unsupported"
    _ -> return ()


writeRender :: MonadWindow m => String -> m ()
writeRender s = do
  (w',h') <- windowSize
  let (w,h) = (itoi w', itoi h')
  ptr <- liftIO $ mallocBytes (4*w*h)
  glReadPixels 0 0 (itoi w') (itoi h') GL_RGBA GL_UNSIGNED_BYTE ptr
  fptr <- liftIO $ newForeignPtr_ ptr
  let img = imageFromUnsafePtr w' h' $ castForeignPtr fptr
  liftIO $ savePngImage s $ ImageRGBA8 img




debugLoop :: (Farbe m) => ([(Event, EventContext)] -> m ()) -> m ()
debugLoop f = (`evalStateT` 0) $ do

  (w',h') <- windowSize
  let (w,h) = (itoi w', itoi h')

  fb <- genFramebuffer
  bindfb fb
  texRGB :: Texture RGB <- loadTexture2Base (w,h) nullPtr
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (texId texRGB) 0
  texD :: Texture D <- loadTexture2Base (w,h) nullPtr
  glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D (texId texD) 0
  framebufferStatus
  bindfb $ Framebuffer 0

  -- ~ t <- makeVarT texD
  -- ~ renderD <- compile $ \v -> do
    -- ~ let V4 x y _ _ = fragCoord
    -- ~ return (up 1 v, texture (use t) $ V2 x y)

  -- ~ t2 <- makeVarT texRGB
  -- ~ renderRGB <- compile $ \v -> do
    -- ~ let V4 x y _ _ = fragCoord
    -- ~ return (up 1 v, texture (use t2) $ V2 x y)

  frame' <- newVArray frame

  t <- makeVarT texD
  render <- renderTexture t

  fix $ \loop -> processEvents $ \es -> do
    case es of
      ((EventKey Key'F9 Down _,_):_) -> modify ((`mod` 2) . succ)
      _ -> return ()
    i <- get

    if i == 0 then do
      lift $ f es
    else do
      bindfb fb
      lift $ f es
      bindfb $ Framebuffer 0
      -- ~ glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT
      render [frame']

    display
    loop

renderTexture :: (MonadIO m, HandTex m)
  => Var (Texture f) -> m ([VArray (V3 Float)] -> m ())
renderTexture t = compile $ \v -> do
  let V4 x y _ _ = fragCoord
  let V4 r g b a = (1 *) $ texture (use t) $ V2 x (-y) * 0.001
  return (up 1 v, V4 r g b 1)


-- ~ renderTexture :: (MonadIO m, HandTex m)
  -- ~ => Var (Texture f) -> m ([VArray (V3 Float)] -> m ())
-- ~ renderTexture t = compile $ \((V3 a b c,V3 x y z)) -> do
    -- ~ let pos = V4 x y z 1
    -- ~ V2 x' y' <- transfer (V2 x (y + a * b * c * 0.00002))
    -- ~ return (pos, texture (use t) ((V2 1 (-0.5))*(V2 x' y')-0.5))


main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do
  i :: Texture RGB <- loadImage' "test-resources/KorDrTtaa4.png"
  t <- makeVarT i

  i2 :: Texture RGB <- loadImage' "test-resources/ayataka512.jpg"
  t2 <- makeVarT i2

  teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
  cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
  -- ~ eiffel <- readFileBinSTL "test-resources/eiffel.stl" >>= newVArray
  r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

  frame' <- newVArray frame

  f <- renderTexture t
  -- ~ f <- compile $ \(n,v) -> do
    -- ~ let v' = use r **| v
    -- ~ n' <- transfer n
    -- ~ return (up 1 v', up 1 n' * 0.5 + 0.2)

  fix $ \loop -> processEvents $ \es -> do
    f [frame']
    display
    loop

  -- ~ liftIO $ putStrLn "waiting"

  -- ~ liftIO $ threadDelay 1000000

  debugLoop $ \es -> do
    -- ~ glClear GL_DEPTH_BUFFER_BIT
    glerrcheck
    case es of
      [(EventMouseMove (x,y),_)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
      _ -> return ()

    -- ~ f [teapot, cube]
    t <- getTime
    -- ~ - ~ when (t > 1 && t < 2) $ do
      -- ~ writeRender "boom.png"

    -- ~ display
    liftIO $ performGC
    -- ~ loop





glerrcheck :: MonadIO m => m ()
glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



