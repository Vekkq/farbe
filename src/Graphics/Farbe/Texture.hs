{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Texture where

import Graphics.Farbe.Utility


import Data.Array.IO
import Data.Array.MArray as MA
import Graphics.Farbe.Vec
import Foreign hiding (void)

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import Graphics.GL.Embedded20
import Graphics.GL.Types




data TexState = TexState
	{ lastUsed :: Word32
	, texArr :: (IOUArray Word32 GLuint) -- Map Word32 (GLuint, String)
	}

initTexState :: MonadIO m => m TexState
initTexState = liftIO $ do
	i <- withPtr_ $ glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	ar <- MA.newArray (1, itoi $ i `quot` 3) 0
	return $ TexState 1 ar


class HandTex m where
	stateTex :: (TexState -> (a, TexState)) -> m a
	getDelayFun :: MonadIO m => m (IO () -> IO ())

getTex :: HandTex m => m TexState
getTex = stateTex (\s -> (s, s))

setTex :: HandTex m => TexState -> m ()
setTex s = stateTex (\_ -> ((), s))


newtype Texture = Texture { tbase :: MVar TextureBase } deriving Eq

getTexId :: MonadIO m => Texture -> m GLuint
getTexId (Texture tb) = liftIO $ texId <$> readMVar tb


-- readonly Texture format
data TextureBase = TextureBase
	{ texId :: GLuint
	, texLastUnit :: GLenum
	-- ~ , changeTokenT :: Int
	, format :: TextureFormat
	, path :: String
	}

data TextureFormat = L | LA | RGB | RGBA | D | TextureFormat
	{ internalFormat :: GLint
	, texFormat :: GLenum
	, texelType :: GLenum
	, postOps :: IO ()
	}

toFormatDef :: TextureFormat -> TextureFormat
toFormatDef L = TextureFormat GL_LUMINANCE GL_LUMINANCE GL_UNSIGNED_BYTE setMipmap
toFormatDef LA = TextureFormat GL_LUMINANCE_ALPHA GL_LUMINANCE_ALPHA GL_UNSIGNED_BYTE setMipmap
toFormatDef RGB = TextureFormat GL_RGB GL_RGB GL_UNSIGNED_BYTE setMipmap
toFormatDef RGBA = TextureFormat GL_RGBA GL_RGBA GL_UNSIGNED_BYTE setMipmap
toFormatDef D = TextureFormat GL_DEPTH_COMPONENT GL_DEPTH_COMPONENT GL_UNSIGNED_BYTE (return ())
toFormatDef tf = tf

type TextureSettings = IO ()

setMipmap :: TextureSettings
setMipmap = do
	glGenerateMipmap GL_TEXTURE_2D
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR

setPixelated :: TextureSettings
setPixelated = do
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
	glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST


glInTex :: TextureFormat -> GLint
glInTex (TextureFormat i _ _ _) = i
glInTex a = glInTex $ toFormatDef a

glTex :: TextureFormat -> GLenum
glTex (TextureFormat _ f _ _) = f
glTex a = glTex $ toFormatDef a

-- ~ texType D = GL_UNSIGNED_SHORT -- only supports Byte according to dev.gl
texType :: TextureFormat -> GLenum
texType (TextureFormat _ _ b _) = b
texType a = texType $ toFormatDef a

texSetup :: TextureFormat -> IO ()
texSetup (TextureFormat _ _ _ io) = io
texSetup a = texSetup $ toFormatDef a

formatL, formatLA, formatRGB, formatRGBA, formatD :: TextureSettings -> TextureFormat
formatL = TextureFormat GL_LUMINANCE GL_LUMINANCE GL_UNSIGNED_BYTE
formatLA = TextureFormat GL_LUMINANCE_ALPHA GL_LUMINANCE_ALPHA GL_UNSIGNED_BYTE
formatRGB = TextureFormat GL_RGB GL_RGB GL_UNSIGNED_BYTE
formatRGBA = TextureFormat GL_RGBA GL_RGBA GL_UNSIGNED_BYTE
formatD = TextureFormat GL_DEPTH_COMPONENT GL_DEPTH_COMPONENT GL_UNSIGNED_BYTE


loadTexture :: forall m a . (MonadIO m, HandTex m)
	=> IO (TextureFormat, V2 GLsizei, Ptr a) -> m Texture
loadTexture io = do
	delay <- getDelayFun
	m <- liftIO newEmptyMVar
	liftIO $ forkIO $ do
		(t, wh, p) <- io
		m2 <- newEmptyMVar
		delay $ do
			tex <- newTexture' t wh p
			putMVar m2 tex
		tex <- takeMVar m2
		putMVar m $ TextureBase tex 0 t ""
		void $ mkWeakMVar m (delay (with tex $ glDeleteTextures 1))
	return $ Texture m


-- returns texture id
newTexture' :: forall m a . (MonadIO m)
	=> TextureFormat -> V2 GLsizei -> Ptr a -> m GLuint
newTexture' t (V2 w h) p = do
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	glActiveTexture GL_TEXTURE0
	glBindTexture GL_TEXTURE_2D tex
	glTexImage2D GL_TEXTURE_2D 0 (glInTex t) w h 0 (glTex t) (texType t) (castPtr p)
	liftIO $ texSetup t
	return tex


newTexture :: forall m a . (MonadIO m, HandTex m)
	=> TextureFormat -> V2 GLsizei -> Ptr a -> m Texture
newTexture t p ptr = do
	i <- newTexture' t p ptr
	m <- liftIO $ newMVar (TextureBase i 0 t "")
	delay <- getDelayFun
	liftIO $ mkWeakMVar m (delay $ with i $ glDeleteTextures 1)
	return $ Texture m


texUpload :: (MonadIO m, HandTex m) => GLint -> Texture -> m ()
texUpload l (Texture t) = do
		tb@(TextureBase i u _ _) <- liftIO $ readMVar t
		-- ~ (TextureBase _ i mu _ _ _) <- liftIO $ readIORef ioreftb
		TexState u' ts <- getTex
		i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
		if (i /= i') then do
			-- ~ liftIO $ putStrLn $ "assigned to unit " ++ show u'
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			liftIO $ catchMVarBlocked 7 $ swapMVar t $ tb { texLastUnit = u'}
			liftIO $ writeArray ts u' i
			u'' <- succU ts u'
			setTex $ TexState u'' ts
		else glUniform1i l $ itoi u
		where
		succU ts x = do
			let x' = succ x -- TODO replace with modulo?
			(a,b) <- liftIO $ getBounds ts
			return $ if x' >= b then a else x'

isTextureLoaded :: MonadIO m => Texture -> m Bool
isTextureLoaded (Texture t) = liftIO $ fmap not $ isEmptyMVar t


