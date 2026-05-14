
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Graphics.Farbe.JuicyPixels where


import Codec.Picture
import Codec.Picture.Types

import Graphics.Farbe.Vec
import Graphics.Farbe.Texture
import Graphics.Farbe.Tuple
-- ~ import Graphics.Farbe.Utils
-- ~ import Graphics.Farbe.GL
import Data.Vector.Storable (unsafeToForeignPtr)
import Control.Monad.IO.Class

import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr



-- ~ loadImage :: (MonadIO m, HandTex m)
  -- ~ => String -> m (Either String Texture)
-- ~ loadImage s = do
  -- ~ ei <- liftIO $ readImage s
  -- ~ mapRight ei $ \i -> do
    -- ~ let (Image w h v) = (toTexture i :: Image f)
    -- ~ let p = unsafeForeignPtrToPtr $ tfst $ unsafeToForeignPtr v
    -- ~ t :: Texture t <- loadTexture2Base (itoi w, itoi h) p
    -- ~ return $ t { path = s }
    -- -- ~ liftIO $ unsafeWith v $ \p -> loadTexture2Base (itoi w, itoi h) p


mapRight :: Applicative f => Either a b -> (b -> f b') -> f (Either a b')
mapRight (Right b) f = Right <$> f b
mapRight (Left a) _ = pure (Left a)


-- ~ loadImage'
  -- ~ :: (MonadIO m, HandTex m, ToTexture f, JuiceTextureFormat t f)
  -- ~ => String -> m Texture
-- ~ loadImage' t s = loadImage s >>= either error return


toGLImage :: DynamicImage -> (TextureFormat, (V2 Int, Ptr ()))
toGLImage i = case convertToGLImage i of
	ImageY8 i -> (L, unpackImage i)
	ImageYA8 i -> (LA, unpackImage i)
	ImageRGB8 i -> (RGB, unpackImage i)
	ImageRGBA8 i -> (RGBA, unpackImage i)
	where
		unpackImage (Image w h v) = (V2 w h, castPtr $ vecToPtr v)
		vecToPtr = unsafeForeignPtrToPtr . tfst . unsafeToForeignPtr


convertToGLImage :: DynamicImage -> DynamicImage
convertToGLImage (ImageY8 i) = ImageY8 i
convertToGLImage (ImageY16 i) = ImageY8 $ trimImage i
convertToGLImage (ImageY32 i) = ImageY8 $ trimImage i
convertToGLImage (ImageYF i) = ImageY8 $ intImage i
convertToGLImage (ImageYA8 i) = ImageYA8 i
convertToGLImage (ImageYA16 i) = ImageYA8 $ trimImage i
convertToGLImage (ImageRGB8 i) = ImageRGB8 i
convertToGLImage (ImageRGB16 i) = ImageRGB8 $ trimImage i
convertToGLImage (ImageRGBF i) = ImageRGB8 $ intImage i
convertToGLImage (ImageRGBA8 i) = ImageRGBA8 i
convertToGLImage (ImageRGBA16 i) = ImageRGBA8 $ trimImage i
convertToGLImage (ImageYCbCr8 i) = ImageRGB8 $ convertImage i
convertToGLImage (ImageCMYK8 i) = ImageRGB8 $ convertImage i
convertToGLImage (ImageCMYK16 i) = ImageRGB8 $ convertImage $ trimImage i


class (Pixel a, Pixel r) => TrimPixel a r | a -> r where
  trimPixel :: a -> r

instance TrimPixel Pixel16 Pixel8 where trimPixel = itoi . (`quot` 2^8)
instance TrimPixel Pixel32 Pixel8 where trimPixel = itoi . (`quot` 2^16)

instance TrimPixel PixelYA16 PixelYA8 where
  trimPixel (PixelYA16 a b) = PixelYA8 (trimPixel a) (trimPixel b)

instance TrimPixel PixelRGB16 PixelRGB8 where
  trimPixel (PixelRGB16 a b c) = PixelRGB8 (trimPixel a) (trimPixel b) (trimPixel c)

instance TrimPixel PixelRGBA16 PixelRGBA8 where
  trimPixel (PixelRGBA16 a b c d) =
    PixelRGBA8 (trimPixel a) (trimPixel b) (trimPixel c) (trimPixel d)

instance TrimPixel PixelCMYK16 PixelCMYK8 where
  trimPixel (PixelCMYK16 a b c d) =
    PixelCMYK8 (trimPixel a) (trimPixel b) (trimPixel c) (trimPixel d)

trimImage :: TrimPixel a b => Image a -> Image b
trimImage = pixelMap trimPixel



class (Pixel a, Pixel r) => IntPixel a r | a -> r, r -> a where
  intPixel :: a -> r

instance IntPixel PixelF Pixel8 where
  intPixel = round . (*256)

instance IntPixel PixelRGBF PixelRGB8 where
  intPixel (PixelRGBF r g b) = PixelRGB8 (intPixel r) (intPixel g) (intPixel b)

intImage :: IntPixel a b => Image a -> Image b
intImage = pixelMap intPixel

