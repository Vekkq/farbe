{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Farbe.Delay where

import Graphics.Farbe.Utility
import Graphics.Farbe.Texture

import Data.Foldable

import Control.Concurrent.MVar

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)


-- Shader delay monad --------------------------------------------------------------------


newtype DelayedT n m a = DelayedT { runGLAction :: DeferT (n ()) m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, Alternative
		, MonadPlus, MonadIO, Count, HandTex
		)

instance (Defer n m, Monad m) => Defer n (DelayedT r m) where
	defer = lift . defer

type DelayedT' m = DelayedT m m

class Delay n m | m -> n where
	delay :: n a -> m (MVar a)

instance (MonadIO n, MonadIO m) => Delay n (DelayedT n m) where
	-- ~ delay :: n a -> DelayedT n m (MVar a)
	delay n = DelayedT $ do
			mvar <- liftIO newEmptyMVar
			defer $ n >>= liftIO . putMVar mvar
			return mvar


liftDelayed :: (Monad m, Delay n m) => DelayedT n m a -> m a
liftDelayed (DelayedT (DeferT ms)) = do
	(a,seq) <- runStateT ms mempty
	mapM_ delay $ toList seq
	return a

-- ~ liftDelayed' :: (Monad m, Delay n m) => DelayedT n o a -> m (o a)
-- ~ liftDelayed' (DelayedT d) = do
	-- ~ (a,seq) <- runDeferT d
	-- ~ undefined -- somethin somethin


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(delay,Delay n,.)


-- ~ type Time = Double

-- ~ type Hash = Int

-- ~ type ShaderFn b = (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))

-- ~ data SKey = SKey Hash String deriving (Eq, Ord)

-- ~ newtype ShaderCacheT m a = ShaderCacheT
	-- ~ { runShaderCacheT :: StateT (Map SKey (Maybe Int)) m a }



-- ~ class ShaderCache m where
	-- ~ shader :: (MonadIO m, HandTex m, AttrType a b)
		-- ~ => (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
		-- ~ -> m (MVar ([VArray a] -> m ()))

-- ~ instance ShaderCache (ShaderCacheT m) where
	-- ~ shader f = do
		-- ~ g <- compile f
		-- ~ m <- newMVar g
		-- ~ return m

