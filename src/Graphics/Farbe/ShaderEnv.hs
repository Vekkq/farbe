{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.ShaderEnv where

import Graphics.Farbe.State
-- ~ import Graphics.Farbe.Shader

import Foreign hiding (void)
import Graphics.GL.Types
import Graphics.GL

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict


type ShaderId = GLuint

data ShaderData = ShaderData
	{ byteCount :: Int
	, byteMax :: Int
	, shaderId :: ShaderId
	, postShaderM :: ShaderEnvT (FarbeT IO) ()
	, preRenderM :: FarbeT IO ()
	, buildSubShader :: [(GLenum, IO ())]
	}

emptyShaderData :: ShaderData
emptyShaderData = ShaderData
	{ byteCount = 0
	, byteMax = error "unset byte max"
	, shaderId = error "unset shader id"
	, postShaderM = return ()
	, preRenderM = return ()
	, buildSubShader = []
	}


newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT ShaderData m a }
	deriving
		(Functor, Applicative, Monad, MonadIO, Farbe)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Functor m) => ShaderEnv m where
	stateShader :: (ShaderData -> (a, ShaderData)) -> m a

	modifyShader :: (ShaderData -> ShaderData) -> m ()
	modifyShader f = stateShader (\s -> ((), f s))

	getsShader :: (ShaderData -> r) -> m r
	getsShader f = f <$> getShader

	getShader :: m (ShaderData)
	getShader = stateShader (\s -> (s, s))

	putShader :: ShaderData -> m ()
	putShader s = stateShader (\_ -> ((),s))

instance Monad m => ShaderEnv (ShaderEnvT m) where
	stateShader = ShaderEnvT . state


runShaderEnvT :: (Monad m) => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = runStateT ms emptyShaderData


createShader :: (MonadIO m, Farbe m)
	=> ShaderEnvT (FarbeT IO) a -> m (a, ShaderData)
createShader ms = do
	sp <- glCreateProgram
	s <- getFarbe
	((a, sd),s') <- liftIO $ runFarbeT s $ runShaderEnvT $ do
		setShaderId sp
		r <- ms
		join $ getsShader postShaderM
		return r
	putFarbe s'
	return $ (a, sd)


liftFarbe :: (Farbe m, MonadIO m) => FarbeT IO a -> m a
liftFarbe m = do
	fd <- getFarbe
	(a,fd') <- liftIO $ runFarbeT fd m
	putFarbe fd'
	return a


-- ~ liftShaderFarbeIO :: (Farbe m, MonadIO m, ShaderEnv (FarbeT IO) m)
	-- ~ => ShaderEnvT (FarbeT IO) a -> m (a, m ())
-- ~ liftShaderFarbeIO ms = do
	-- ~ fd <- getFarbe
	-- ~ sd <- getShader
	-- ~ (a, exec) <- liftFarbe $ runShaderEnvT $ putFarbe fd >> putShader sd >> ms
	-- ~ let exec' = liftFarbe exec
	-- ~ return $ (a, exec')


postShader :: (ShaderEnv m) => ShaderEnvT (FarbeT IO) () -> m ()
postShader m = modifyShader (\s -> s { postShaderM = postShaderM s >> m } )

preRender :: (ShaderEnv m) => FarbeT IO () -> m ()
preRender m = modifyShader (\s -> s { preRenderM = preRenderM s >> m } )

modifyByteCount :: (ShaderEnv m) => (Int -> Int) -> m ()
modifyByteCount f = modifyShader (\s -> s { byteCount = f $ byteCount s } )

advanceBy :: (Monad m, ShaderEnv m, Storable s) => s -> m Int
advanceBy a = do
	i <- getsShader byteCount
	modifyByteCount (sizeOf a +)
	return i

setByteMax :: (ShaderEnv m) => Int -> m ()
setByteMax i = modifyShader (\s -> s { byteMax = i } )

getByteMax :: (ShaderEnv m) => m Int
getByteMax = getsShader byteMax

setShaderId :: ShaderEnv m => ShaderId -> m ()
setShaderId i = modifyShader (\s -> s { shaderId = i })

getShaderId :: ShaderEnv m => m ShaderId
getShaderId = getsShader shaderId

addSubShader :: ShaderEnv m => GLenum -> IO () -> m ()
addSubShader e io = modifyShader $
	\s -> s { buildSubShader = buildSubShader s ++ [(e,io)] }



