{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- ~ {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Graphics.Farbe.Shader where

import Graphics.Farbe.Expr
import Graphics.Farbe.Vec
import Graphics.Farbe.GL
import Graphics.Farbe.Attribute
import Graphics.Farbe.VertexArray
-- ~ import Graphics.Farbe.State
-- ~ import Graphics.Farbe.BuildShader
-- ~ import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Utility
-- ~ import Graphics.Farbe.Expr

import Data.Char
import Data.List
import Data.Foldable
import Data.Hashable
import Foreign hiding (void)
import Foreign.C
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.IntMap as M
import qualified Data.Set as S

import Data.Dynamic
import Numeric

import Graphics.GL.Embedded20
import Graphics.GL.Types
import Graphics.GL.Ext.OES.VertexArrayObject

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict

#define bottom undefined





--- ShdrState - Saving global shaders ----------------------------------------------------

data ShdrState = ShdrState { shdrMap :: M.IntMap Dynamic }

class Monad m => HandShdr m where
	stateShdr :: (ShdrState -> (a, ShdrState)) -> m a

	-- ~ delayShdr :: m ((ShdrState -> ShdrState) -> IO ())

getShdr :: HandShdr m => m ShdrState
getShdr = stateShdr (\s -> (s, s))

setShdr :: HandShdr m => ShdrState -> m ()
setShdr s = stateShdr (\_ -> ((), s))


-- SHADER DEFINITION ---------------------------------------------------------------------

getShader :: Shader m f g => f -> m g
getShader = undefined

isShaderCompiled :: f -> m Bool
isShaderCompiled = undefined


-- ~ shader
	-- ~ :: (Shader f g, ShaderEnv m, HandShdr n
	-- ~ , LiftF (m Bool -> n Bool) g h)
	-- ~ => f -> n h
-- ~ shader = undefined

ful
	:: (V3 (Expr V Float))
	-> (V4 (Expr V Float), V4 (Expr F Float))
ful v = let
	v' = v
	n' = transfer' v
	in (up 1 v', up 1 n' * 0.5 + 0.2)

ful' :: (MonadIO m, HandShdr m) => m (VArray (V3 Float) -> m Bool)
ful' = makeShader ful


makeShader :: (MonadIO m, Shader (ShaderEnvT m) f g) => f -> m g
makeShader f = do
	(g, sd) <- runShaderEnvT $ mkShader f
	return g

class ShaderEnv m => Shader m f g | g -> f where
	mkShader :: f -> m g

instance (Shader m f g, AppliableF m g, ShaderEnv m, MonadIO m)
	=> Shader m
	(Mat V3 V3 (Expr V Float) -> f)
	(Mat V3 V3 Float -> g) where
	-- ~ mkShader :: (Mat V3 V3 (Expr V Float) -> f) -> m (Mat V3 V3 Float -> g)
	mkShader f = do
		s <- getsShader shaderId
		let vname = "foo"
		g <- mkShader $ f (matParts $ Expr $ ExprI vname (TVec3 $ TVec3 TFloat) [] RegisterUniform)
		l <- withString vname $ glGetUniformLocation s
		-- ~ return $ \m -> applyF g $ when (l > 0) $ modifyShader $ \sd -> sd { preRender = upload l m >> preRender sd }
		return $ \m -> applyF g $ when (l > 0) $ upload l m


	-- ~ makeShader f = makeShader (f ()) $ \g -> applyF $ do
		-- ~ i <- getShaderId
		-- ~ n <- getName
		-- ~ postShader $ do
			-- ~ upload n g

instance (Attribute a b, MonadIO m, ShaderEnv m)
	=> Shader m
	(b -> (V4 (Expr V Float), V4 (Expr F Float)))
	(VArray a -> n Bool) where
	mkShader f = undefined -- do
	-- ~ (vaoId,e) <- setAttributes (bottom :: a)
		-- ~ compile f


-- ~ class JoinF m f g where
	-- ~ joinF :: f -> g



class LiftF nm f g | f nm -> g, g nm -> f where
	liftF :: nm -> f -> g

instance {-# INCOHERENT #-} LiftF nm f g => LiftF nm (a -> f) (a -> g) where
	liftF nm f = \a -> liftF nm (f a)

instance LiftF (n -> m) n m where
	liftF nm n = nm n


class AppliableF m f | f -> m where
	applyF :: f -> m a -> f

instance AppliableF m b => AppliableF m (a -> b) where
	applyF f m = \p -> applyF (f p) m

instance Applicative m => AppliableF m (m a) where
	applyF f m = m *> f



class ComposeF m f where
	composef :: m f -> f

instance {-# INCOHERENT #-} (Functor m, ComposeF m b) => ComposeF m (a -> b) where
	composef m = \a -> composef (fmap ($ a) m)

instance Monad m => ComposeF m (m a) where
	composef = join


-- Upload --------------------------------------------------------------------------------

class (GLtype a, Eq a, MonadIO m) => Upload m a where
	upload :: GLint -> a -> m ()

instance MonadIO m => Upload m Bool where upload l = glUniform1i l . boolToInt
instance MonadIO m => Upload m Int32 where upload l = glUniform1i l . itoi
instance MonadIO m => Upload m Float where	upload l = glUniform1f l
instance MonadIO m => Upload m (V2 Float) where upload l (V2 a b) = glUniform2f l a b
instance MonadIO m => Upload m (V3 Float) where upload l (V3 a b c) = glUniform3f l a b c
instance MonadIO m => Upload m (V4 Float) where upload l (V4 a b c d) = glUniform4f l a b c d

instance MonadIO m => Upload m (V2 Int32) where
	upload l (V2 a b) = glUniform2i l (itoi a) (itoi b)

instance MonadIO m => Upload m (V3 Int32) where
	upload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)

instance MonadIO m => Upload m (V4 Int32) where
	upload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)

instance MonadIO m => Upload m (V2 Bool) where
	upload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)

instance MonadIO m => Upload m (V3 Bool) where
	upload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)

instance MonadIO m => Upload m (V4 Bool) where
	upload l (V4 a b c d) =
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)


instance MonadIO m => Upload m (Mat V2 V2 Float) where
	upload l = (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance MonadIO m => Upload m (Mat V3 V3 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p

instance MonadIO m => Upload m (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p

withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray

(.:) = (.).(.)

-- (HandTex m) =>

------------------------------------------------------------------------------------------

-- ~ type ShaderId = GLuint

data ShaderType = Vertex | Fragment

data ShaderData = ShaderData
	{ counter :: Int
	, shaderId :: ShaderId
	, headers :: S.Set (ShaderType, Header)
	, exprs :: S.Set (ShaderType, ExprI)
	, preRender :: IO ()
	}

emptyShaderData :: MonadIO m => m ShaderData
emptyShaderData = do
	i<- glCreateProgram
	return $ ShaderData
		{ counter = 0
		, shaderId = i
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
getsShader f = f <$> getShader'

getShader' :: ShaderEnv m => m (ShaderData)
getShader' = stateShader (\s -> (s, s))

putShader :: ShaderEnv m => ShaderData -> m ()
putShader s = stateShader (\_ -> ((),s))



instance Monad m => ShaderEnv (ShaderEnvT m) where
	stateShader = ShaderEnvT . state

runShaderEnvT :: MonadIO m => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = emptyShaderData >>= runStateT ms


data Header



transfer' :: V3 (Expr V Float) -> V3 (Expr F Float)
transfer' = undefined

transfer :: Expr V a -> Expr F a -- extend to all variants
transfer = undefined



