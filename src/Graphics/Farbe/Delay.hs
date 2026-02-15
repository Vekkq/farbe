{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Farbe.Delay where

import Graphics.Farbe.Utility
import Graphics.Farbe.Texture
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Window

import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence ((|>))

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

type DSeq n = S.Seq (n ())

newtype DelayedT n m a = DelayedT { unDelayedT :: StateT (DSeq n) m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, Alternative
		, MonadPlus, MonadIO, Count, HandTex, HandVBO, Defer d
		, MonadReader r, MonadWriter w
		, MonadError e, MonadWindow
		)

instance MonadState s m => MonadState s (DelayedT n m) where
	get = lift get
	put = lift . put

-- ~ instance (Defer n m, Monad m) => Defer n (DelayedT r m) where
	-- ~ defer = lift . defer

type DelayedT' m = DelayedT m m

runDelayedT :: Monad m => DelayedT n m a -> m (a, DSeq n)
runDelayedT (DelayedT m) = do
	(a,w) <- runStateT m S.empty
	return (a, w)


class Monad m => DelayedState n m where
	delayedState :: (DSeq n -> (a, DSeq n)) -> m a

	delayedStateGet :: m (DSeq n)
	delayedStateGet = delayedState $ \s -> (s,s)
	delayedStatePut :: DSeq n -> m ()
	delayedStatePut a = delayedState $ \_ -> ((),a)


instance Monad m => DelayedState n (DelayedT n m) where
	delayedState = DelayedT . state




class Delay n m | m -> n where
	delay :: n a -> m ()

instance (MonadIO n, MonadIO m) => Delay n (DelayedT n m) where
	-- ~ delay :: n a -> DelayedT n m (MVar a)
	delay n = DelayedT $ modify (|>void n)

delayR n = do
		mvar <- liftIO newEmptyMVar
		delay $ n >>= liftIO . putMVar mvar
		return mvar

-- ~ liftDelayed :: (Monad m, Delay n m) => DelayedT n m a -> m a
-- ~ liftDelayed (DelayedT (DeferT ms)) = do
	-- ~ (a,seq) <- runStateT ms mempty
	-- ~ mapM_ delay $ toList seq
	-- ~ return a

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

