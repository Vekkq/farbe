{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Graphics.Farbe.JuicyPixels where


import Codec.Picture
import Codec.Picture.Types

import Graphics.Farbe
import Graphics.Farbe.Vec ()
import Graphics.Farbe.Texture
import Graphics.Farbe.Tuple
import Graphics.Farbe.BuildShader
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Utility
import Graphics.Farbe.GL
import Data.Vector.Storable (unsafeToForeignPtr)

import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Data.Either
import Control.Monad



loadImage :: (MonadIO m, Farbe m) => String -> m Texture
loadImage s = loadTexture $ do
		ei <- readImage s
		let (format, (dim,ptr)) = toGLImage $ fromRight (ImageRGB8 errorTexture) ei
		either print (void . return) ei -- add debug command
		return (format, dim, ptr)


errorTexture :: Image PixelRGB8
errorTexture = generateImage f 8 8
	where
	f x y = if odd $ x + y then PixelRGB8 255 100 200 else PixelRGB8 255 255 0


mapRight :: Applicative f => Either a b -> (b -> f b') -> f (Either a b')
mapRight (Right b) f = Right <$> f b
mapRight (Left a) _ = pure (Left a)


textureIO :: String -> V2 (Expr e Float) -> V4 (Expr e Float)
textureIO str p = flip texture p $ Expr $ ExprI shdr TTex []
	where
		vname = sani str
		shdr = do
			b <- addHeader "uniform" (undefined :: Texture) vname
			s <- getShaderId
			when b $ postShader $ do
				t <- loadImage str
				l <- withString vname $ glGetUniformLocation s
				preRender $ do
					b1 <- isTextureLoaded t
					when b1 $ texUpload l t
					return b1
			return vname


sani :: [Char] -> [Char]
sani = ("t_"++)
	. filter (\x -> elem x $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
	. replace (\a -> if elem a "\\/.-" then '_' else a)


replace :: (a -> b) -> [a] -> [b]
replace _ [] = []
replace f xs = foldr (\a ys -> f a : ys) [] xs


toGLImage :: DynamicImage -> (TextureFormat, (V2 GLsizei, Ptr ()))
toGLImage j = case convertToGLImage j of
	ImageY8 i -> (L, unpackImage i)
	ImageYA8 i -> (LA, unpackImage i)
	ImageRGB8 i -> (RGB, unpackImage i)
	ImageRGBA8 i -> (RGBA, unpackImage i)
	_ -> undefined
	where
		unpackImage (Image w h v) = (itoi <$> V2 w h, castPtr $ vecToPtr v)
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

