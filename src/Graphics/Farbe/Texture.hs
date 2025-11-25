{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Texture where




import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Function
import Data.Foldable
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Data.Array.MArray as MA
import Numeric
import Foreign hiding (void)
import Foreign.C

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Cont
import Control.Monad.RWS
import Data.List
import Graphics.Farbe.Vec
import Graphics.GL
import Graphics.GL.Embedded20
-- ~ import Graphics.Farbe.Utils
-- ~ import Graphics.Farbe.GL
import Graphics.Farbe.Window
import Data.Vector.Storable (unsafeWith)
import Control.Monad.IO.Class




newtype HandTexT m a = HandTexT { unTex :: StateT TexState m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadFix, MonadPlus, MonadWindow --, PostShaderProgram, PreRender
		)

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

runHandTex :: MonadIO m => HandTexT m a -> m a
runHandTex (HandTexT m) = do
	t <- initTexState
	evalStateT m t

joinHandTex :: (MonadIO m, HandTex m) => HandTexT m a -> m a
joinHandTex (HandTexT m) = do
	t <- getTex
	(a,s) <- runStateT m t
	setTex s
	return a


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
	, texLastUnit :: GLenum
	, changeTokenT :: Int
	, width :: GLsizei
	, height :: GLsizei
	} deriving Eq

instance Show (Texture f) where
	show = show . texId


data TextureFormat = L | LA | RGB | RGBA

glTex L = GL_LUMINANCE
glTex LA = GL_LUMINANCE_ALPHA
glTex RGB = GL_RGB
glTex RGBA = GL_RGBA


-- @loadTexture2Base@ requires an image with width and height at base of 2 .
loadTexture2Base :: MonadIO m
	=> TextureFormat -> (GLsizei, GLsizei) -> Ptr a -> m (Texture t)
loadTexture2Base t (w,h) p = do
	tex <- liftIO $ withPtr_ $ glGenTextures 1
	glActiveTexture $ GL_TEXTURE0
	glBindTexture GL_TEXTURE_2D tex
	glTexImage2D GL_TEXTURE_2D 0 (glTex t) w h 0 (glTex t) GL_UNSIGNED_BYTE (castPtr p)
	glGenerateMipmap GL_TEXTURE_2D
	return $ Texture tex 0 0 w h


withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m (a, b)
withPtr f = liftIO $ alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr_ f = fst <$> withPtr f

-- ~ makeVarT :: MonadGL m => Texture t -> m (Var (Texture t))
-- ~ makeVarT = makeVar

-- ~ instance GLtype (Texture f) where
	-- ~ glCName _ = "sampler2D"
	-- ~ glType _ = GL_INT
	-- ~ glPrecision _ = ""
	-- ~ setupUpload l m = preRender $ do
		-- ~ (Texture i u c w h) <- liftIO $ readMVar m -- borked TODO
		-- ~ mts <- texUnits <$> glState
		-- ~ (u', ts) <- liftIO $ readMVar mts
		-- ~ i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
		-- ~ when (i /= i') $ do
			-- ~ glActiveTexture $ GL_TEXTURE0 + u'
			-- ~ glBindTexture GL_TEXTURE_2D i
			-- ~ glUniform1i l $ itoi u'
			-- ~ liftIO $ swapMVar m $ Texture i u' c w h
			-- ~ u'' <- succU ts u'
			-- ~ liftIO $ writeArray ts u'' i
			-- ~ liftIO $ void $ swapMVar mts (u'',ts)
	-- ~ glShortName _ = "t"

-- ~ succU ts x = do
	-- ~ let x' = succ x
	-- ~ (l,h) <- liftIO $ getBounds ts
	-- ~ return $ if x' >= h then l else x'


-- ~ instance Use (Var (Texture f)) e (Expr e (Texture f)) where
  -- ~ use = Expr . varAst

-- add expr texture shader access functions

-- ~ texture :: Expr e (Texture f) -> V2 (Expr e Float) -> V4 (Expr e Float)
-- ~ texture t v = vecParts $ liftE2 "texture2D" t (exprVec v)
