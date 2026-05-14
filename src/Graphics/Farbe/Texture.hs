{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Texture where

import Graphics.Farbe.GL
import Graphics.Farbe.Utility
-- ~ import Graphics.Farbe.Uniform


import Data.Array.IO
import Data.Array.MArray as MA
import Foreign hiding (void)

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.RWS
import Graphics.Farbe.Vec
import Graphics.GL



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

	delayedTex :: (MonadIO m, MonadIO n) => n () -> m ()

getTex :: m TexState
getTex = stateTex (\s -> (s, s))
setTex :: TexState -> m ()
setTex s = stateTex (\_ -> ((), s))


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(stateTex,HandTex,.)

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
	} deriving Eq

data L
data LA
data RGB
data RGBA
data D

data TextureFormat = L | LA | RGB | RGBA | D deriving (Eq,Show,Read)

glTex L = GL_LUMINANCE
glTex LA = GL_LUMINANCE_ALPHA
glTex RGB = GL_RGB
glTex RGBA = GL_RGBA
glTex D = GL_DEPTH_COMPONENT

glInTex L = GL_ALPHA
glInTex LA = GL_LUMINANCE_ALPHA
glInTex RGB = GL_RGB
glInTex RGBA = GL_RGBA
glInTex D = GL_DEPTH_COMPONENT

-- ~ texType D = GL_UNSIGNED_SHORT -- only supports Byte according to dev.gl
texType _ = GL_UNSIGNED_BYTE

texSetup :: MonadIO m => TextureFormat -> m ()
texSetup D = return ()
texSetup _ = glGenerateMipmap GL_TEXTURE_2D

-- returns texture id and assigned texture unit
loadTexture' :: forall m t a . (MonadIO m)
	=> TextureFormat -> V2 GLsizei -> Ptr a -> m GLuint
loadTexture' t (V2 w h) p = do
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	-- ~ m <- assignTexUnit' tex 0
	glActiveTexture GL_TEXTURE0
	glBindTexture GL_TEXTURE_2D tex
	glTexImage2D GL_TEXTURE_2D 0 (glInTex t) w h 0 (glTex t) (texType t) (castPtr p)
	texSetup t
	return tex
	-- ~ liftIO $ void $ mkWeakMVar m (with tex $ glDeleteTextures 1)
	-- TODO wait for bufferswap before deleting
	-- also ensure its send to the main thread


loadTexture :: forall m t a . (MonadIO m, HandTex m)
	=> TextureFormat -> V2 GLsizei -> Ptr a -> m Texture
loadTexture t p ptr = do
	i <- loadTexture' t p ptr
	liftIO $ Texture <$> newMVar (TextureBase i 0 t "")

	-- ~ return $ Texture tex m w h ""


assignTexUnit' :: (MonadIO m, HandTex m, Num n) => GLuint -> GLenum -> m n
assignTexUnit' i u = do
	TexState l ts <- getTex
	i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
	if (i /= i') then do
		glActiveTexture $ GL_TEXTURE0 + l
		glBindTexture GL_TEXTURE_2D i
		liftIO $ writeArray ts l i
		l' <- succU ts l
		setTex $ TexState l' ts
		return $ itoi l
	else return $ itoi u
	where
	succU ts x = do
		let x' = succ x
		(a,b) <- liftIO $ getBounds ts
		return $ if x' >= b then a else x'

assignTexUnit :: (MonadIO m, HandTex m) => Texture -> m ()
assignTexUnit (Texture mtb) = do
	tb@(TextureBase i u _ _) <- liftIO $ takeMVar mtb
	u' <- assignTexUnit' i u
	liftIO $ putMVar mtb $ tb { texLastUnit = u' }




instance GLtype Texture where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"


