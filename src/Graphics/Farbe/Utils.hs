{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Utils where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import Graphics.Farbe.Texture
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Window




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
	if maybe False (a==) l
		then return ()
		else do
			fuzzySwapMVar ml a
			f a


updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ fuzzySwapMVar m a

fuzzySwapMVar :: MonadIO m => MVar a -> a -> m (Maybe a)
fuzzySwapMVar ml a = liftIO $ do
	r <- tryTakeMVar ml
	tryPutMVar ml a
	return r


-- DeferT --------------------------------------------------------------------------------

newtype DeferT m a = DeferT { unDefer :: WriterT [m ()] m a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadPlus, MonadIO, Count, HandTex
		)

instance MonadTrans DeferT where
	lift = DeferT . lift


runDeferT :: Monad m => DeferT m a -> m (a, m ())
runDeferT m = do
	(a,w) <- runWriterT $ unDefer m
	return $ (a, sequence_ w)

runDeferT' :: Monad m => DeferT m a -> m a
runDeferT' m = do
	(a,e) <- runDeferT m
	e
	return a

class Monad m => Defer n m | m -> n where
	defer :: n () -> m ()

instance Monad m => Defer m (DeferT m) where
	defer = DeferT . tell . (:[])


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(defer,Defer n,.)


-- CounterT ------------------------------------------------------------------------------

newtype CounterT m a = CounterT { counter :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadFix, MonadPlus, Defer m, HandTex, HandVBO, MonadWindow
		)

instance MonadState s m => MonadState s (CounterT m) where
	get = lift get
	put = lift . put


instance Monad m => Semigroup (CounterT m a) where
	(<>) = (>>)

instance Monad m => Monoid (CounterT m ()) where
	mempty = return ()

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
generateName s = count >>= return . (s++) . ("_"++) . show





