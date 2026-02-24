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

data ShaderData = ShaderData
	{ byteCount :: Int
	, byteMax :: Int
	, shaderId :: ShaderId
	, postShaderM :: ShaderEnvT (FarbeT IO) ()
	, preRenderM :: FarbeT IO ()
	}

emptyShaderData :: ShaderData
emptyShaderData = ShaderData
	{ byteCount = 0
	, byteMax = error "unset byte max"
	, postShaderM = return ()
	, preRenderM = return ()
	, shaderId = error "unset shader id"
	}

newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT ShaderData m a }
	deriving
		(Functor, Applicative, Monad, MonadIO, Farbe)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Functor n) => ShaderEnv m n | n -> m where
	stateShader :: (ShaderData -> (a, ShaderData)) -> n a

	modifyShader :: (ShaderData -> ShaderData) -> n ()
	modifyShader f = stateShader (\s -> ((), f s))

	getsShader :: (ShaderData -> r) -> n r
	getsShader f = f <$> getShader

	getShader :: n (ShaderData)
	getShader = stateShader (\s -> (s, s))

	putShader :: ShaderData -> n ()
	putShader s = stateShader (\_ -> ((),s))

instance Monad m => ShaderEnv m (ShaderEnvT m) where
	stateShader = ShaderEnvT . state

runShaderEnvT :: (MonadIO m, Farbe m) => ShaderEnvT m a -> m (a, m ())
runShaderEnvT ms = do
	(a,sd) <- runShaderEnvT' $ do
		a <- ms
		getsShader postShaderM
		return a
	return (a, liftFarbe $ preRenderM sd)

runShaderEnvT'' :: (MonadIO m, Farbe m) => ShaderEnvT (FarbeT IO) a -> m (a, m ())
runShaderEnvT'' ms = undefined
	-- ~ (a,sd) <- runShaderEnvT' $ do
		-- ~ a <- ms
		-- ~ getsShader postShaderM
		-- ~ return a
	-- ~ return (a, liftFarbe $ preRenderM sd)

runShaderEnvT' :: (Monad m) => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT' (ShaderEnvT ms) = runStateT ms emptyShaderData


liftFarbe :: (Farbe m, MonadIO m) => FarbeT IO a -> m a
liftFarbe m = do
	fd <- getFarbe
	(a,fd') <- liftIO $ runFarbeT' fd m
	putFarbe fd'
	return a


liftShaderFarbeIO :: (Farbe m, MonadIO m, ShaderEnv (FarbeT IO) m)
	=> ShaderEnvT (FarbeT IO) a -> m (a, m ())
liftShaderFarbeIO ms = do
	fd <- getFarbe
	sd <- getShader
	(a, exec) <- liftFarbe $ runShaderEnvT $ putFarbe fd >> putShader sd >> ms
	let exec' = liftFarbe exec
	return $ (a, exec')


postShader :: (ShaderEnv n m) => ShaderEnvT (FarbeT IO) () -> m ()
postShader m = modifyShader (\s -> s { postShaderM = postShaderM s >> m } )

preRender :: (ShaderEnv n m) => FarbeT IO () -> m ()
preRender m = modifyShader (\s -> s { preRenderM = preRenderM s >> m } )

modifyByteCount :: (ShaderEnv n m) => (Int -> Int) -> m ()
modifyByteCount f = modifyShader (\s -> s { byteCount = f $ byteCount s } )

advanceBy :: (Monad m, ShaderEnv n m, Storable s) => s -> m Int
advanceBy a = do
	i <- getsShader byteCount
	modifyByteCount (sizeOf a +)
	return i

setByteMax :: (ShaderEnv n m) => Int -> m ()
setByteMax i = modifyShader (\s -> s { byteMax = i } )

getByteMax :: (ShaderEnv n m) => m Int
getByteMax = getsShader byteMax


getShaderId :: ShaderEnv n m => m ShaderId
getShaderId = getsShader shaderId




