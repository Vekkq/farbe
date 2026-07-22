{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.ShaderEnv where

-- ~ import Graphics.Farbe.State
import Foreign hiding (void)
import Graphics.GL

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Set as S
import Graphics.Farbe.Expr


data ShaderType = Vertex | Fragment

type Header = String

type ShaderId = GLuint

data ShaderData = ShaderData
	{ shaderId :: ShaderId
	, vaoByteCount :: Int
	, vaoByteMax :: Int
	, headers :: S.Set (ShaderType, Header)
	, exprs :: S.Set (ShaderType, ExprI)
	, preRender :: IO ()
	}

emptyShaderData :: MonadIO m => m ShaderData
emptyShaderData = do
	i<- glCreateProgram
	return $ ShaderData
		{ shaderId = i
		, vaoByteCount = 0
		, vaoByteMax = error "not defined"
		, headers = S.empty
		, exprs = S.empty
		, preRender = return ()
		}


newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: StateT ShaderData m a }
	deriving
		(Functor, Applicative, Monad, MonadIO)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift

instance MonadState s m => MonadState s (ShaderEnvT m) where
	state = lift . state


class (Functor m) => ShaderEnv m where
	stateShader :: (ShaderData -> (a, ShaderData)) -> m a

modifyShader :: ShaderEnv m => (ShaderData -> ShaderData) -> m ()
modifyShader f = stateShader (\s -> ((), f s))

getsShader :: ShaderEnv m => (ShaderData -> r) -> m r
getsShader f = f <$> getShader

getShader :: ShaderEnv m => m (ShaderData)
getShader = stateShader (\s -> (s, s))

putShader :: ShaderEnv m => ShaderData -> m ()
putShader s = stateShader (\_ -> ((),s))

instance Monad m => ShaderEnv (ShaderEnvT m) where
	stateShader = ShaderEnvT . state


runShaderEnvT :: MonadIO m => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = emptyShaderData >>= runStateT ms


createShader :: MonadIO m => ShaderEnvT m a -> m (a, ShaderData)
createShader ms = do
	sp <- glCreateProgram
	runShaderEnvT ms
	-- ~ (a, sd) <- liftIO $
		-- ~ setShaderId sp
		-- ~ r <- ms
		-- ~ join $ getsShader postShaderM
		-- ~ sd <- getShader
		-- ~ putShader $ sd { postShaderM = return () }
		-- ~ return r
	-- ~ return $ (a, sd)


modifyByteCount :: (ShaderEnv m) => (Int -> Int) -> m ()
modifyByteCount f = modifyShader (\s -> s { vaoByteCount = f $ vaoByteCount s } )

advanceBy :: (Monad m, ShaderEnv m, Storable s) => s -> m Int
advanceBy a = do
	i <- getsShader vaoByteCount
	modifyByteCount (sizeOf a +)
	return i

setByteMax :: (ShaderEnv m) => Int -> m ()
setByteMax i = modifyShader (\s -> s { vaoByteMax = i } )

getByteMax :: (ShaderEnv m) => m Int
getByteMax = getsShader vaoByteMax

-- ~ setShaderId :: ShaderEnv m => ShaderId -> m ()
-- ~ setShaderId i = modifyShader (\s -> s { shaderId = i })

getShaderId :: ShaderEnv m => m ShaderId
getShaderId = getsShader shaderId


-- ~ addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m Bool
-- ~ addHeader i a n = do
	-- ~ let str = unwords [i, slNameWithPrec a, n, ";"]
	-- ~ s <- buildShaderStateGet
	-- ~ let b = not $ S.member str (header s)
	-- ~ when b $ buildShaderStatePut $ s { header = S.insert str $ header s }
	-- ~ return b


-- ~ addExpr :: String -> Expr e a -> Shdr ()
-- ~ addExpr n (Expr a) = do
	-- ~ e <- runExprI a
	-- ~ buildShaderState $ \s -> ((), s { bexpr = (n,e) : bexpr s })

