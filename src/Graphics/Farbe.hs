{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe
	( runFarbeT
	, Farbe (..)
	, newVArray
	, frame
	, compile
	, transfer
	, use
	, module Graphics.Farbe.Vec
	, module Graphics.Farbe.Expr
	, makeVar
	, makeVarF
	, makeVarI
	, makeVarB
	, makeVarV2F
	, makeVarV2I
	, makeVarV2B
	, makeVarV3F
	, makeVarV3I
	, makeVarV3B
	, makeVarV4F
	, makeVarV4I
	, makeVarV4B
	, makeVarM2
	, makeVarM3
	, makeVarM4
	, makeVarT

	) where

import Graphics.Farbe.Vec
import Graphics.Farbe.Shader
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.Utils
import Graphics.Farbe.Expr
import Graphics.Farbe.GL
import Graphics.Farbe.Window
import Control.Monad.IO.Class
import Data.Int
import Foreign.Storable

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS

import Graphics.GL.Embedded20
import Graphics.GL.Types


newtype FarbeT m a = FarbeT { unFarbe :: CounterT (HandTexT (HandVBOT m)) a }
	deriving
		( Functor, Applicative, Monad, MonadIO
		, Count, HandTex, HandVBO
		, MonadReader r, MonadState s, MonadWriter w
		, MonadError e, MonadWindow
		)

instance MonadTrans FarbeT where
	lift = FarbeT . lift . lift . lift

-- ~ deriving instance (Monad m) => Count (WindowT m)

class (Count m, HandTex m, HandVBO m, MonadWindow m, MonadIO m) => Farbe m
instance (MonadWindow m, MonadIO m) => Farbe (FarbeT m)

instance (Farbe m, Monad m) => Farbe (ReaderT r m)
instance (Farbe m, Monad m, Monoid w) => Farbe (WriterT w m)
instance (Farbe m, Monad m) => Farbe (StateT r m)
instance (Farbe m, Monad m) => Farbe (ExceptT r m)
instance (Farbe m, Monad m, Monoid w) => Farbe (RWST r w s m)

instance MonadIO m => MonadFail (FarbeT m) where
	fail s = liftIO $ do
		return $ error s


runFarbeT :: MonadIO m => FarbeT m a -> m a
runFarbeT m = runHandVBOT (2^24) . runHandTexT . runCounterT' . unFarbe $ do
	glClearColor 0.1 0.1 0.1 1
	glEnable GL_DEPTH_TEST
	glPixelStorei GL_UNPACK_ALIGNMENT 1
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE

	-- ~ glEnable GL_CULL_FACE
	m


-- ~ runFarbeT' :: MonadIO m => WindowT (FarbeT m) a -> m a
-- ~ runFarbeT' = runFarbeT . runWindowT "" (InWindow (1000,1024))





data Render
	= DrawShader (forall m . FarbeT m ())
	| DrawOver Render Render
	| DrawInto Render Render
	| Draws [Render]

display' :: Farbe m => Render -> FarbeT m ()

display' (DrawShader m) = m

display' (Draws ms) = mapM_ display' ms
	
display' (DrawOver a b) = do
	glEnable GL_STENCIL_TEST
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_REPLACE
	-- ~ glStencilOp GL_KEEP GL_KEEP GL_DECR_WRAP
	display' a
	glStencilFunc GL_GREATER 1 1
	display' b
	glDisable GL_STENCIL_TEST
	
display' (DrawInto a b) = do
	glEnable GL_STENCIL_TEST
	glClear GL_STENCIL_BUFFER_BIT
	glStencilOp GL_KEEP GL_DECR_WRAP GL_DECR_WRAP
	glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
	display' a
	glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
	
	glStencilOp GL_KEEP GL_KEEP GL_KEEP
	glStencilFunc GL_LESS 1 0xFF
	glDisable GL_DEPTH_TEST
	display' b
	
	glStencilFunc GL_ALWAYS 0 0xFF
	glDisable GL_STENCIL_TEST
		
		
drawTexture :: Render -> m (Texture RGBA)
drawTexture = undefined

drawDepth :: Render -> m (Texture D)
drawDepth = undefined

compile' = undefined -- :: attribsandall => m Render

