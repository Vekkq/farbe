{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.ShaderEnv where

import Graphics.Farbe.Window
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.State
import Graphics.Farbe.GL
-- ~ import Graphics.Farbe.Shader

import Data.Map
import Data.Dynamic
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

import System.Mem.StableName
import Control.Exception
import Control.Concurrent.MVar

import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.RWS

import Control.Monad.IO.Class


type ShaderId = GLuint

data ShaderData w = ShaderData
	{ byteCount :: Int
	, byteMax :: Int
	, counter :: Int
	, shaderId :: ShaderId
	, postShaderM :: ShaderEnvT w ()
	, preRenderM :: w ()
	}

emptyShaderData :: Monad w => ShaderData w
emptyShaderData = ShaderData
	{ byteCount = 0
	, byteMax = error "unset byte max"
	, counter = 0
	, postShaderM = return ()
	, preRenderM = return ()
	, shaderId = error "unset shader id"
	}

newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT (ShaderData m) m a }
	deriving
		(Functor, Applicative, Monad, MonadIO)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Functor n) => ShaderEnv m n | n -> m where
	stateShader :: (ShaderData m -> (a, ShaderData m)) -> n a

	modifyShader :: (ShaderData m -> ShaderData m) -> n ()
	modifyShader f = stateShader (\s -> ((), f s))

	getsShader :: (ShaderData m -> r) -> n r
	getsShader f = f <$> getShader

	getShader :: n (ShaderData m)
	getShader = stateShader (\s -> (s, s))

	putShader :: ShaderData m -> n ()
	putShader s = stateShader (\_ -> ((),s))

instance Monad m => ShaderEnv m (ShaderEnvT m) where
	stateShader = ShaderEnvT . state

runShaderEnvT :: (Monad m, Farbe m) => ShaderEnvT m a -> m (a, ShaderData m)
runShaderEnvT (ShaderEnvT ms) = runStateT ms emptyShaderData

liftShaderEnv :: ShaderEnv n m => ShaderEnvT n m a -> m a
liftShaderEnv = do
	sd <- getShader
	(a,sd') <- runShaderEnvT $ putShader sd >>= env
	putShader sd
	return a

liftShaderFarbe :: (Farbe m, MonadIO m) => FarbeT IO a -> m a
liftShaderFarbe m = do
	fd <- farbeGet
	(a,fd') <- liftIO $ runFarbeT' fd m
	farbePut fd'
	return a

liftShaderEnvTFarbeIO :: (Monad m) => ShaderEnvT (FarbeT IO) a -> m (a, m ())
liftShaderEnvTFarbeIO ms = do
	(a, dat) <- runShaderEnvT $ do
								r <- liftFarbe ms
								getsShader postShaderM
								return r
	return $ (a, preRenderM dat)

count :: ShaderEnv n m => m Int
count = stateShader (\s -> let c = counter s in (c, s { counter = succ c }))

postShader :: (ShaderEnv n m, Monad n) => ShaderEnvT n () -> m ()
postShader m = modifyShader (\s -> s { postShaderM = postShaderM s >> m } )

preRender :: (ShaderEnv n m, Monad n) => n () -> m ()
preRender m = modifyShader (\s -> s { preRenderM = preRenderM s >> m } )

modifyByteCount :: (ShaderEnv n m) => (Int -> Int) -> m ()
modifyByteCount f = modifyShader (\s -> s { byteCount = f $ byteCount s } )

advanceBy :: (Monad m, ShaderEnv n m, Storable s) => s -> m Int
advanceBy a = do
	i <- getsShader byteCount
	modifyByteCount (sizeOf a +)
	return i

setByteMax :: (Monad m, ShaderEnv n m) => Int -> m ()
setByteMax i = modifyShader (\s -> s { byteMax = i } )

getByteMax :: (Monad m, ShaderEnv n m) => m Int
getByteMax = getsShader byteMax


getShaderId :: ShaderEnv n m => m ShaderId
getShaderId = getsShader shaderId




