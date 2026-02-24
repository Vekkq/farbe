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
-- ~ import Graphics.Farbe.Utils
-- ~ import Graphics.Farbe.GL

data TexState = TexState
	{ lastUsed :: Word32
	, texArr :: (IOUArray Word32 GLuint)
	}

initTexState :: MonadIO m => m TexState
initTexState = liftIO $ do
	i <- withPtr_ $ glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	ar <- MA.newArray (1, itoi $ i `quot` 3) 0
	return $ TexState 1 ar

-- ~ evalHandTexT :: MonadIO m => HandTexT m a -> m a
-- ~ evalHandTexT (HandTexT m) = do
	-- ~ t <- initTexState
	-- ~ evalStateT m t

-- ~ runHandTexT :: MonadIO m => HandTexT m a -> m a
-- ~ runHandTexT (HandTexT m) = initTexState >>= evalStateT m

-- ~ runHandTexT' :: MonadIO m => TexState -> HandTexT m a -> m (a, TexState)
-- ~ runHandTexT' s (HandTexT m) = runStateT m s

-- ~ joinHandTex :: (MonadIO m, HandTex m) => HandTexT m a -> m a
-- ~ joinHandTex (HandTexT m) = do
	-- ~ t <- getTex
	-- ~ (a,s) <- runStateT m t
	-- ~ setTex s
	-- ~ return a


class HandTex m where
	stateTex :: (TexState -> (a, TexState)) -> m a

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


-- ~ liftHandTexT :: (HandTex m, MonadIO m) => HandTexT m a -> m a
-- ~ liftHandTexT n = do
	-- ~ t <- getTex
	-- ~ (r,t') <- runHandTexT' t n
	-- ~ setTex t'
	-- ~ return r

-- ~ liftHandTexT' :: (HandTex m, MonadIO m) => HandTexT IO a -> m a
-- ~ liftHandTexT' n = do
	-- ~ t <- getTex
	-- ~ (r,t') <- liftIO $ runHandTexT' t n
	-- ~ setTex t'
	-- ~ return r




data Texture f = Texture
	{ texId :: GLuint
	, texLastUnit :: MVar GLenum
	, changeTokenT :: Int
	, width :: GLsizei
	, height :: GLsizei
	} deriving Eq


instance Show (Texture f) where
	show = show . texId


data L
data LA
data RGB
data RGBA
data D

class TextureFormat a where
	glTex :: (Eq n, Num n) => a -> n
	glInTex :: (Eq n, Num n) => a -> n
	glTexType :: (Eq n, Num n) => a -> n
	glTexType _ = GL_UNSIGNED_BYTE
	glMipMap :: a -> Bool
	glMipMap _ = True

-- ~ instance TextureFormat D where glTex _ = GL_DEPTH_COMPONENT24
-- ~ instance TextureFormat S where glTex _ = GL_LUMINANCE
instance TextureFormat L where
	glTex _ = GL_LUMINANCE --GL_RED
	glInTex _ = GL_ALPHA --GL_R8

instance TextureFormat LA where
	glTex _ = GL_LUMINANCE_ALPHA --GL_RG
	glInTex _ = GL_LUMINANCE_ALPHA --GL_RG8

instance TextureFormat RGB where
	glTex _ = GL_RGB
	glInTex _ = GL_RGB --GL_RGB8

instance TextureFormat RGBA where
	glTex _ = GL_RGBA
	glInTex _ = GL_RGBA --GL_RGBA8

instance TextureFormat D where
	glTex _ = GL_DEPTH_COMPONENT
	glInTex _ = GL_DEPTH_COMPONENT
	glTexType _ = GL_UNSIGNED_SHORT --GL_UNSIGNED_INT
	glMipMap _ = False

-- @loadTexture2Base@ requires an image with width and height at base of 2 .
loadTexture2Base :: forall m t a . (MonadIO m, HandTex m, TextureFormat t)
	=> (GLsizei, GLsizei) -> Ptr a -> m (Texture t)
loadTexture2Base (w,h) p = do
	let t = (error "" :: t)
	liftIO $ print (w,h)
	-- ~ let int = glInTex (error "" :: t)
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	m <- assignTexUnit' tex 0 >>= (liftIO . newMVar)
	glTexImage2D GL_TEXTURE_2D 0 (glInTex t) w h 0 (glTex t) (glTexType t) (castPtr p)
	when (glMipMap t) $ glGenerateMipmap GL_TEXTURE_2D
	-- ~ when (p /= nullptr) $ glGenerateMipmap GL_TEXTURE_2D

	liftIO $ mkWeakMVar m (with tex $ glDeleteTextures 1)
	-- TODO wait for bufferswap before deleting

	return $ Texture tex m 0 w h

{-
-- @loadTexture2Base@ requires an image with width and height at base of 2 .
loadTexture2Base :: MonadIO m
	=> TextureFormat -> (GLsizei, GLsizei) -> Ptr a -> m (Texture t)
loadTexture2Base t (w,h) p = do
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	-- ~ liftIO $ putStrLn $ "new tex: " ++ show tex
	glActiveTexture $ GL_TEXTURE0
	glBindTexture GL_TEXTURE_2D tex
	glTexImage2D GL_TEXTURE_2D 0 (glTex t) w h 0 (glTex t) GL_UNSIGNED_BYTE (castPtr p)
	glGenerateMipmap GL_TEXTURE_2D
	m <- liftIO $ newMVar 0
	liftIO $ mkWeakMVar m (with tex $ glDeleteTextures 1)
	-- todo wait for bufferswap before deleting

	return $ Texture tex m 0 w h
-}


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

assignTexUnit :: (MonadIO m, HandTex m) => Texture f -> m ()
assignTexUnit (Texture i mu _ _ _) = do
	u <- liftIO $ takeMVar mu
	u' <- assignTexUnit' i u
	liftIO $ putMVar mu u'




instance GLtype (Texture f) where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"


