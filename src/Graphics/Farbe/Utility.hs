{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Utility where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
import Graphics.Farbe.Utils
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Array
import Graphics.Farbe.Texture
import Graphics.Farbe.Window


import Data.Char
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import qualified Data.Sequence as S
import Data.Sequence ((|>))




import Graphics.GL.Embedded20
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats

import Debug.Trace



#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

-- RunOnce -------------------------------------------------------------------------------

data RunOnce m a = RunOnce (m a) (MVar a)

makeRunOnce :: MonadIO m => m2 a -> m (RunOnce m2 a)
makeRunOnce ma = liftIO $ RunOnce ma <$> newEmptyMVar

runOnce :: MonadIO m => RunOnce m a -> m a
runOnce (RunOnce m ma) = do
	maybea <- liftIO $ tryReadMVar ma
	case maybea of
		Just a -> return a
		Nothing -> do
			b <- m
			liftIO $ tryPutMVar ma b
			return b

makeRunOnce' :: MonadIO m => m a -> m (m a)
makeRunOnce' m = do
	ro <- makeRunOnce m
	return $ runOnce ro


-- RunWhenChanged ------------------------------------------------------------------------

data RunWhenChanged m a = RunWhenChanged (a -> m ()) (MVar a)

makeRunWhenChanged :: MonadIO m => (a -> m2 ()) -> m (RunWhenChanged m2 a)
makeRunWhenChanged m = liftIO $ RunWhenChanged m <$> newEmptyMVar

runwc :: (MonadIO m, Eq a) => RunWhenChanged m a -> a -> m ()
runwc (RunWhenChanged f ml) a = do
	l <- liftIO $ tryReadMVar ml
	when (Just a /= l) $ do
		fuzzySwapMVar ml a
		f a

updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ fuzzySwapMVar m a

fuzzySwapMVar :: MonadIO m => MVar a -> a -> m (Maybe a)
fuzzySwapMVar ml a = liftIO $ do
	r <- tryTakeMVar ml
	putMVar ml a
	return r


-- DeferT --------------------------------------------------------------------------------
-- | DeferT, simple monad to defer monadic operations.

newtype DeferT n m a = DeferT { unDefer :: StateT (S.Seq n) m a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadPlus, MonadIO, Count, HandTex
		)

type DeferT' m = DeferT (m ()) m

instance MonadTrans (DeferT n) where
	lift = DeferT . lift


runDeferT :: (Monad m) => DeferT n m a -> m (a, [n])
runDeferT m = do
	(a,w) <- runStateT (unDefer m) S.empty
	return (a, toList w)

runDeferT' :: Monad m => DeferT' m a -> m a
runDeferT' m = do
	(a,e) <- runDeferT m
	sequence_ e
	return a

runDeferT'' :: Monad m => DeferT n m a -> m (a, [n])
runDeferT'' m = do
	(a,w) <- runStateT (unDefer m) S.empty
	return (a, toList w)

class Monad m => Defer n m | m -> n where
	defer :: n -> m ()

instance (Monad m) => Defer n (DeferT n m) where
	defer = DeferT . (\a -> modify (|>a))

SIMPLEFUNCTION_CLASSINSTANCES(defer,Defer n,.)


-- ~ class Monad m => GetDeferred n m | m -> n where
	-- ~ getDeferred :: m (Maybe (n ()))

-- ~ instance (Monad m) => GetDeferred n (DeferT (n ()) m) where
	-- ~ getDeferred = DeferT $ do
		-- ~ seq <- get
		-- ~ put $ S.drop 1 seq
		-- ~ return $ S.lookup 0 seq

-- ~ SIMPLEFUNCTION_CLASSINSTANCES(getDeferred,GetDeferred n,)



-- CounterT ------------------------------------------------------------------------------

newtype CounterT m a = CounterT { counter :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadPlus, Defer n, HandTex, HandVBO, MonadWindow
		)

instance MonadState s m => MonadState s (CounterT m) where
	get = lift get
	put = lift . put

-- ~ instance Monad m => Semigroup (CounterT m a) where (<>) = (>>)
-- ~ instance Monad m => Monoid (CounterT m ()) where mempty = return ()

class Monad m => Count m where
	count :: m Int

instance Monad m => Count (CounterT m) where
	count = CounterT $ state $ \s -> (s, succ s)

SIMPLEFUNCTION_CLASSINSTANCES(count,Count,)

runCounterT :: Monad m => Int -> CounterT m a -> m a
runCounterT i (CounterT st) = evalStateT st i

runCounterT' :: Monad m => CounterT m a -> m a
runCounterT' = runCounterT 1

generateName :: Count m => String -> m String
generateName s = (s++) . ("_"++) . show <$> count

