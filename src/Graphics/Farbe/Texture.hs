
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe.Texture where


import Codec.Picture
import Codec.Picture.Types

import Data.List
import Graphics.Farbe.Vec
import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.Farbe
import Graphics.Farbe.Utils
import Data.Vector.Storable (unsafeWith)
import Control.Monad.IO.Class






loadImage :: (MonadIO m, TextureFormat t) => t -> String -> m (Either String (Texture t))
loadImage t s = do
  ei <- liftIO $ readImage s
  right ei $ \i -> do
    let (Image w h v) = toRGB i
    liftIO $ unsafeWith v $ \p -> loadTexture2Base t (itoi w, itoi h) p


right :: Applicative f => Either a b -> (b -> f b') -> f (Either a b')
right (Right b) f = Right <$> f b
right (Left a) _ = pure (Left a)


toRGB :: DynamicImage -> Image PixelRGB8
toRGB (ImageY8 i) = promoteImage i
toRGB (ImageY16 i) = promoteImage $ trimImage i
toRGB (ImageY32 i) = promoteImage $ trimImage i
toRGB (ImageYF i) = promoteImage $ intImage i
toRGB (ImageYA8 i) = promoteImage i
toRGB (ImageYA16 i) = promoteImage $ trimImage i
toRGB (ImageRGB8 i) = i
toRGB (ImageRGB16 i) = trimImage i
toRGB (ImageRGBF i) = intImage i
toRGB (ImageRGBA8 i) = collapseImage i
toRGB (ImageRGBA16 i) = collapseImage $ trimImage i
toRGB (ImageYCbCr8 i) = convertImage i
toRGB (ImageCMYK8 i) = convertImage i
toRGB (ImageCMYK16 i) = convertImage $ trimImage i


toRGBA :: DynamicImage -> Image PixelRGBA8
toRGBA (ImageY8 i) = promoteImage i
toRGBA (ImageY16 i) = promoteImage $ trimImage i
toRGBA (ImageY32 i) = promoteImage $ trimImage i
toRGBA (ImageYF i) = promoteImage $ intImage i
toRGBA (ImageYA8 i) = promoteImage i
toRGBA (ImageYA16 i) = promoteImage $ trimImage i
toRGBA (ImageRGB8 i) = promoteImage i
toRGBA (ImageRGB16 i) = promoteImage $ trimImage i
toRGBA (ImageRGBF i) = promoteImage $ intImage i
toRGBA (ImageRGBA8 i) = i
toRGBA (ImageRGBA16 i) = trimImage i
toRGBA (ImageYCbCr8 i) = promoteImageRGBA $ convertImage i
toRGBA (ImageCMYK8 i) = promoteImageRGBA $ convertImage i
toRGBA (ImageCMYK16 i) = promoteImageRGBA $ convertImage $ trimImage i

promoteImageRGBA :: Image PixelRGB8 -> Image PixelRGBA8
promoteImageRGBA = promoteImage


toY :: DynamicImage -> Image Pixel8
toY (ImageY8 i) = i
toY (ImageY16 i) = trimImage i
toY (ImageY32 i) = trimImage i
toY (ImageYF i) = intImage i
toY (ImageYA8 i) = collapseImage i
toY (ImageYA16 i) = collapseImage $ trimImage i
toY (ImageRGB8 i) = collapseImage i
toY (ImageRGB16 i) = collapseImage $ trimImage i
toY (ImageRGBF i) = collapseImage $ intImage i
toY (ImageRGBA8 i) = collapseImage i
toY (ImageRGBA16 i) = collapseImage $ trimImage i
toY (ImageYCbCr8 i) =
  collapseImage $ (convertImage :: Image PixelYCbCr8 -> Image PixelRGB8) i
toY (ImageCMYK8 i) =
  collapseImage $ (convertImage :: Image PixelCMYK8 -> Image PixelRGB8) i
toY (ImageCMYK16 i) =
  collapseImage $ (convertImage :: Image PixelCMYK8 -> Image PixelRGB8) $ trimImage i


class (Pixel a, Pixel r) => TrimPixel a r | a -> r where
  trimPixel :: a -> r

instance TrimPixel Pixel16 Pixel8 where trimPixel = itoi . (`quot` 2^8)
instance TrimPixel Pixel32 Pixel8 where trimPixel = itoi . (`quot` 2^16)
-- ~ instance TrimPixel Pixel32 Pixel16 where trimPixel = itoi . (`quot` 2^8)

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


averageInt xs = itoi $ sum (map itoi xs) `quot` length xs

class (Pixel a, Pixel r) => CollapsePixel a r where
  collapsePixel :: a -> r

instance CollapsePixel PixelYA8 Pixel8 where
  collapsePixel (PixelYA8 y a) = itoi $ averageInt [y,a]

instance CollapsePixel PixelRGB8 Pixel8 where
  collapsePixel (PixelRGB8 r g b) = itoi $ averageInt [r,g,b]

instance CollapsePixel PixelRGBA8 PixelRGB8 where
  collapsePixel (PixelRGBA8 r g b _) = PixelRGB8 r g b

instance CollapsePixel PixelRGBA8 Pixel8 where
  collapsePixel = (collapsePixel :: (PixelRGB8 -> Pixel8)) . collapsePixel

collapseImage :: CollapsePixel a b => Image a -> Image b
collapseImage = pixelMap collapsePixel


class (Pixel a, Pixel r) => IntPixel a r | a -> r, r -> a where
  intPixel :: a -> r

instance IntPixel PixelF Pixel8 where
  intPixel = round . (*256)

instance IntPixel PixelRGBF PixelRGB8 where
  intPixel (PixelRGBF r g b) = PixelRGB8 (intPixel r) (intPixel g) (intPixel b)

intImage :: IntPixel a b => Image a -> Image b
intImage = pixelMap intPixel

-- ~ toRGBA :: DynamicImage -> Image PixelRGBA8
-- ~ toRGBA (ImageY8 i) = promoteImage i
-- ~ toRGBA (ImageY16 i) = promoteImage $ trimImage i
-- ~ toRGBA (ImageY32 i) = promoteImage $ trimImage i
-- ~ toRGBA (ImageYF i) = undefined
-- ~ toRGBA (ImageYA8 i) = promoteImage i
-- ~ toRGBA (ImageYA16 i) = promoteImage $ trimImage i
-- ~ toRGBA (ImageRGB8 i) = i
-- ~ toRGBA (ImageRGB16 i) = trimImage i
-- ~ toRGBA (ImageRGBF i) = undefined
-- ~ toRGBA (ImageRGBA8 i) = collapseImage i
-- ~ toRGBA (ImageRGBA16 i) = collapseImage $ trimImage i
-- ~ toRGBA (ImageYCbCr8 i) = convertImage i
-- ~ toRGBA (ImageCMYK8 i) = convertImage i
-- ~ toRGBA (ImageCMYK16 i) = convertImage $ trimImage i


