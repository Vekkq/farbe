{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

getShader :: Shader f g => f -> m g
getShader = undefined

isShaderCompiled :: f -> m Bool
isShaderCompiled = undefined


-- ~ shader
	-- ~ :: (Shader f g, ShaderEnv m, HandShdr n
	-- ~ , LiftF (m Bool -> n Bool) g h)
	-- ~ => f -> n h
-- ~ shader = undefined


makeShader :: (Shader f g) => f -> g
makeShader f = do
	-- ~ (a, sd) <- runShaderEnvT $ do
		-- ~ i <- getsShader shaderId

		-- ~ undefined
		-- ~ setAttributes i a
	-- ~ mkShader f
	undefined

class Shader f g | g -> f where
	mkShader :: ShaderEnv m => f -> m g

instance (Shader f g, AppliableF m g) => Shader
	(Mat V3 V3 (Expr V Float) -> f)
	(Mat V3 V3 Float -> g) where
	-- ~ makeShader f = makeShader (f ()) $ \g -> applyF $ do
		-- ~ i <- getShaderId
		-- ~ n <- getName
		-- ~ postShader $ do
			-- ~ upload n g

instance (Attribute a b, Monad m) => Shader
	(b -> (V4 (Expr V Float), V4 (Expr F Float)))
	(VArray a -> m Bool) where
	mkShader = undefined -- do
	-- ~ (vaoId,e) <- setAttributes (bottom :: a)


class LiftF nm f g | f nm -> g, g nm -> f where
	liftF :: nm -> f -> g

instance {-# INCOHERENT #-} LiftF nm f g => LiftF nm (a -> f) (a -> g) where
	liftF nm f = \a -> liftF nm (f a)

instance LiftF (n -> m) n m where
	liftF nm n = nm n


class AppliableF m f | f -> m where
	applyF :: f -> m a -> f

instance {-# INCOHERENT #-} AppliableF m b => AppliableF m (a -> b) where
	applyF f m = \p -> applyF (f p) m

instance Applicative m => AppliableF m (m a) where
	applyF f m = m *> f



class ComposeF m f where
	composef :: m f -> f

instance {-# INCOHERENT #-} (Functor m, ComposeF m b) => ComposeF m (a -> b) where
	composef m = \a -> composef (fmap ($ a) m)

instance Monad m => ComposeF m (m a) where
	composef = join

-- VAO LAND ------------------------------------------------------------------------------

-- ~ class Storable a => Attribute a b | a -> b, b -> a where
	-- ~ setAttribute' :: a -> m b

-- ~ instance Attribute (V3 Float, V3 Float) (V3 (Expr V Float), V3 (Expr V Float))

------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------

-- ~ type ShaderId = GLuint

data ShaderType = Vertex | Fragment

data ShaderData = ShaderData
	{ counter :: Int
	, shaderId :: ShaderId
	, headers :: S.Set (ShaderType, Header)
	, exprs :: S.Set (ShaderType, ExprI)
	}

emptyShaderData :: MonadIO m => m ShaderData
emptyShaderData = do
	i<- glCreateProgram
	return $ ShaderData
		{ counter = 0
		, shaderId = i
		, headers = S.empty
		, exprs = S.empty
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

runShaderEnvT :: (MonadIO m) => ShaderEnvT m a -> m (a, ShaderData)
runShaderEnvT (ShaderEnvT ms) = emptyShaderData >>= runStateT ms


data Header



transfer' :: V3 (Expr V Float) -> V3 (Expr F Float)
transfer' = undefined

transfer :: Expr V a -> Expr F a -- extend to all variants
transfer = undefined



