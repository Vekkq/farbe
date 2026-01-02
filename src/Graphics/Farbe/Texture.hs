{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Texture where

import Graphics.Farbe.GL
import Graphics.Farbe.Utils
-- ~ import Graphics.Farbe.Uniform


import Data.Array.IO
import Data.Array.MArray as MA
import Foreign hiding (void)

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Cont
import Control.Monad.RWS
import Graphics.Farbe.Vec
import Graphics.GL
-- ~ import Graphics.Farbe.Utils
-- ~ import Graphics.Farbe.GL
import Graphics.Farbe.Window


import Graphics.Farbe.VertexArray (HandVBO)


newtype HandTexT m a = HandTexT { unTex :: StateT TexState m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO, HandVBO
		, MonadFix, MonadPlus, MonadWindow
		)

instance Monad m => Semigroup (HandTexT m a) where
	(<>) = (>>)

instance Monad m => Monoid (HandTexT m a) where
	mempty = return $ error ""


instance MonadState s m => MonadState s (HandTexT m) where
	get = lift get
	put = lift . put

data TexState = TexState
	{ lastUsed :: Word32
	, texArr :: (IOUArray Word32 GLuint)
	}

initTexState :: MonadIO m => m TexState
initTexState = liftIO $ do
	i <- withPtr_ $ glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	ar <- MA.newArray (1, itoi $ i `quot` 3) 0
	return $ TexState 1 ar

evalHandTexT :: MonadIO m => HandTexT m a -> m a
evalHandTexT (HandTexT m) = do
	t <- initTexState
	evalStateT m t

runHandTexT :: MonadIO m => HandTexT m a -> m a
runHandTexT (HandTexT m) = initTexState >>= evalStateT m

runHandTexT' :: MonadIO m => TexState -> HandTexT m a -> m (a, TexState)
runHandTexT' s (HandTexT m) = runStateT m s

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


instance Monad m => HandTex (HandTexT m) where
	stateTex = HandTexT . state

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(stateTex,HandTex,.)


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

class TextureFormat a where
	glTex :: (Eq n, Num n) => a -> n

instance TextureFormat L where glTex _ = GL_LUMINANCE
instance TextureFormat LA where glTex _ = GL_LUMINANCE_ALPHA
instance TextureFormat RGB where glTex _ = GL_RGB
instance TextureFormat RGBA where glTex _ = GL_RGBA

-- @loadTexture2Base@ requires an image with width and height at base of 2 .
loadTexture2Base :: forall m t a . (MonadIO m, TextureFormat t)
	=> (GLsizei, GLsizei) -> Ptr a -> m (Texture t)
loadTexture2Base (w,h) p = do
	let t = glTex (error "" :: t)
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	glActiveTexture $ GL_TEXTURE0
	glBindTexture GL_TEXTURE_2D tex
	glTexImage2D GL_TEXTURE_2D 0 (itoi t) w h 0 t GL_UNSIGNED_BYTE (castPtr p)
	glGenerateMipmap GL_TEXTURE_2D
	-- ~ when (p /= nullptr) $ glGenerateMipmap GL_TEXTURE_2D

	m <- liftIO $ newMVar 0
	liftIO $ mkWeakMVar m (with tex $ glDeleteTextures 1)
	-- todo wait for bufferswap before deleting

	return $ Texture tex m 0 w h



instance GLtype (Texture f) where
	slName _ = "sampler2D"
	toTypeS _ = TTex
	glType _ = GL_INT
	glPrecision _ = ""
	glShortName _ = "t"


