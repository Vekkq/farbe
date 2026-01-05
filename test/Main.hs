
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
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

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



writeRender :: MonadWindow m => String -> m ()
writeRender s = do
  (w',h') <- windowSize
  let (w,h) = (itoi w', itoi h')
  ptr <- liftIO $ mallocBytes (4*w*h)
  glReadPixels 0 0 (itoi w') (itoi h') GL_RGBA GL_UNSIGNED_BYTE ptr
  fptr <- liftIO $ newForeignPtr_ ptr
  let img = imageFromUnsafePtr w' h' $ castForeignPtr fptr
  liftIO $ savePngImage s $ ImageRGBA8 img




processEventsDebug :: (Farbe m) => ([(Event, EventContext)] -> m ()) -> m ()
processEventsDebug f = (`evalStateT` 1) $ do

  (w',h') <- windowSize
  let (w,h) = (itoi w', itoi h')

  fb <- genFramebuffer
  bindfb fb
  texRGB :: Texture RGB <- loadTexture2Base (w,h) nullPtr
  assignTexUnit texRGB
  -- ~ glTexParameteri (texId texRGB) GL_TEXTURE_MIN_FILTER GL_LINEAR
  -- ~ glTexParameteri (texId texRGB) GL_TEXTURE_MAG_FILTER GL_LINEAR
  glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D (texId texRGB) 0
  -- ~ texD :: Texture RGB <- loadTexture2Base (w,h) nullPtr
  -- ~ assignTexUnit texD
  -- ~ glFramebufferTexture2D GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_TEXTURE_2D  (texId texD) 0
  -- ~ texS :: Texture RGB <- loadTexture2Base (w,h) nullPtr
  -- ~ assignTexUnit texS
  -- ~ glFramebufferTexture2D GL_FRAMEBUFFER GL_STENCIL_ATTACHMENT GL_TEXTURE_2D  (texId texS) 0
  -- ~ framebufferStatus
  t <- makeVarT texRGB
  render <- compile $ \v -> do
    let V4 x y _ _ = fragCoord
    return (up 1 v, texture (use t) $ V2 x y)

  frame' <- newVArray frame

  fix $ \loop -> processEvents $ \es -> do
    bindfb fb
    lift $ f es
    case es of
      ((EventKey Key'F12 Down _,_):_) -> modify ((`mod` 3) . succ)
      _ -> return ()
    i <- get
    -- ~ swapVar t $ [texRGB, texD, texS] !! i
    swapVar t texRGB
    bindfb (Framebuffer 0)

    -- ~ glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT
    render [frame']
    display
    loop




main :: IO ()
main = runWindowT "" (InWindow (1000,1024)) $ runFarbeT $ do
  i :: Texture RGB <- loadImage' "test-resources/iwi.jpg"
  t <- makeVarT i

  i2 :: Texture RGB <- loadImage' "test-resources/ayataka512.jpg"
  t2 <- makeVarT i2

  teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
  cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray
  eiffel <- readFileBinSTL "test-resources/eiffel.stl" >>= newVArray

  r <- makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

  f <- compile $ \(n,v) -> do
    let v' = use r **| v
    n' <- transfer n
    return (up 1 v', up 1 n' * 0.5 + 0.2)

  processEventsDebug $ \es -> do
    -- ~ glClear GL_DEPTH_BUFFER_BIT
    glerrcheck
    r' <- readVar r
    case es of
      [(EventMouseMove (x,y),_)] -> void $ swapVar r $ rotationMatrix 0 (x*0.01) (y*0.01)
      _ -> return ()

    f [eiffel, teapot, cube]
    t <- getTime
    -- ~ when (t > 1 && t < 2) $ do
      -- ~ writeRender "boom.png"

    -- ~ display
    liftIO $ performGC
    -- ~ loop





glerrcheck :: MonadIO m => m ()
glerrcheck = liftIO $ glGetError >>= \e -> when (e/=0) $ putStrLn $ "gl error: " ++ show e



