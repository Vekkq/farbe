{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.VertexArray where

import Graphics.Farbe.Vec
import Graphics.Farbe.Window
import Graphics.Farbe.MState


import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Foreign hiding (void)
import Foreign.C

import System.Mem.Weak

-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

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

import Debug.Trace


newtype HandVBOT m a = HandVBOT { unHandVBO :: MStateT HandVBOState m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadWindow
		)

instance MonadState s m => MonadState s (HandVBOT m) where
	get = lift get
	put = lift . put

runHandVBOT :: MonadIO m => GLintptr -> HandVBOT m a -> m a
runHandVBOT i (HandVBOT m) = do
	s <- initHandVBOState i
	runMStateT s m

runHandVBOT' :: MonadIO m => HandVBOState -> HandVBOT m a -> m HandVBOState
runHandVBOT' a (HandVBOT m) = runMStateT a $ m >> get

class (MonadIO m) => HandVBO m where
	stateHandVBO :: (HandVBOState -> (a, HandVBOState)) -> m a

	getHandVBO :: m HandVBOState
	getHandVBO = stateHandVBO (\s -> (s, s))

	setHandVBO :: HandVBOState -> m ()
	setHandVBO s = stateHandVBO (\_ -> ((), s))

	getVBOMVar :: m (MVar HandVBOState)

instance (MonadIO m) => HandVBO (HandVBOT m) where
	stateHandVBO = HandVBOT . state
	getVBOMVar = HandVBOT getMVar

#define SIMPLEFUNCTION2_CLASSINSTANCES(fn,op,fn2,op2,cn) \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn; fn2 = lift op2 fn2 }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn; fn2 = lift op2 fn2 }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn; fn2 = lift op2 fn2 } ;\

SIMPLEFUNCTION2_CLASSINSTANCES(stateHandVBO,.,getVBOMVar,,HandVBO)

-- VBO manager ---------------------------------------------------------------------------

data HandVBOState = HandVBOState
	{ pager :: Pager GLintptr
	, vboIndex :: GLuint
	}

initHandVBOState :: MonadIO m => GLintptr -> m HandVBOState
initHandVBOState s = liftIO $ do
	vboMan <- withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER vboMan
	glBufferData GL_ARRAY_BUFFER s nullPtr GL_STATIC_DRAW
	return $ HandVBOState (newPager s) vboMan

vboUpdate :: (MonadIO m, Storable a) => VArrayF a -> StorableArray Int a -> m ()
vboUpdate (VArrayF s i) a =
	liftIO $ withStorableArray a $ \p -> glBufferSubData GL_ARRAY_BUFFER i s $ castPtr p


vboAlloc :: HandVBO m => GLintptr -> GLintptr -> m GLintptr
vboAlloc a i = do
	pager <- pager <$> getHandVBO
	let maybeP = calcAlloc a pager i
	case maybeP of
		Just (pager', p) -> do
			putPager pager'
			return p
		Nothing -> do
			liftIO $ putStrLn $
				"The vboMan exceeded its allocated size. Recovering."
				++ "Consider to increase its default. "
			vboRecover
			vboAlloc a i

vboRecover :: HandVBO m => m ()
vboRecover = do
	pager <- pager <$> getHandVBO
	let size = fst $ M.findMax $ imap pager
	let newSize = size*2
	oldvbo <- vboIndex <$> getHandVBO
	p <- liftIO $ withPtr_ $ glGetBufferPointervOES GL_ARRAY_BUFFER GL_BUFFER_MAP_POINTER_OES
	v <- liftIO $ withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER v
	glBufferData GL_ARRAY_BUFFER newSize p GL_STATIC_DRAW
	deleteBuffer oldvbo
	mvm <- getHandVBO
	let pager' = pager { imap = fixKey size newSize $ imap pager }
	setHandVBO $ HandVBOState pager' v
	return ()
	where
		fixKey o n m = M.insert n (negate n) $ M.delete o m

		deleteBuffer :: MonadIO m => GLuint -> m ()
		deleteBuffer i = liftIO $ alloca $ \p -> do
			poke p i
			glDeleteBuffers 1 p


vboFree :: HandVBO m => GLintptr -> m ()
vboFree a = updatePager $ \p -> return $ (,()) $ calcRemove a p


-- | Merge neighboring ranges
condense :: (Eq n, Num n) => [(n,n)] -> [(n,n)]
condense (x@(xp,xr):y@(yp,yr):xs)
	| xp+xr == yp = condense $ (xp, xr+yr):xs
	| otherwise = x : condense (y:xs)
condense (x:_) = [x]
condense [] = []

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x,y) = (f x, f y)

vArrayRange :: VArrayF a -> (CPtrdiff, CPtrdiff)
vArrayRange (VArrayF s p) = (p,s)

drawRanges :: [VArrayF a] -> [(CPtrdiff, CPtrdiff)]
drawRanges = condense . map vArrayRange . sortBy (comparing vArrayPos)

withPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m (a, b)
withPtr f = liftIO $ alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: (MonadIO m, Storable a) => (Ptr a -> IO ()) -> m a
withPtr_ f = fst <$> withPtr f


-- Pager - calculations for Buffer allocation --------------------------------------------

data Pager n = Pager
	{ imap :: M.Map n n -- | position - length
	, lastCheck :: n
	} deriving (Read, Show, Eq, Ord)


newPager :: Integral n
	=> n -- | total size
	-> Pager n
newPager s = Pager (M.fromList [((-1), 1), (s,negate s)]) 0

updatePager :: HandVBO m => (Pager GLintptr -> m (Pager GLintptr, a)) -> m a
updatePager f = do
	vm <- getHandVBO
	(p',r) <- f $ pager vm
	setHandVBO $ vm { pager = p' }
	return r

putPager :: HandVBO m => Pager GLintptr -> m ()
putPager a = updatePager $ \_ -> return (a, ())

calcAlloc :: Integral n
	=> n -- | alignment
	-> Pager n
	-> n -- | size to be allocated
	-> Maybe (Pager n, n)
calcAlloc a mm@(Pager imap c) size
	| size > 0 = (\i -> (Pager (M.insert i size imap) (i+size), i)) <$> nextSpace a mm start size
	| otherwise = Nothing
	where
		start = fromMaybe (error "Pager: corrupted") $ uncurry (+) <$> M.lookupLE c imap

align :: (Integral a) => a -> a -> a
align a p
	| a <= 0 = p
	| mod p a == 0 = p
	| otherwise = p + a - mod p a

nextSpace :: Integral n => n -> Pager n -> n -> n -> Maybe n
nextSpace a (Pager imap c) start size =
	case (M.lookupLE c imap, M.lookupGE c imap) of
		(Just (p,l), Just (p2,l2))
			| aln <- (align a $ p+l), aln + size <= p2 -> Just aln
			| p+l == start -> Nothing
			| otherwise -> nextSpace a (Pager imap (p2+l2)) start size
		_ -> error "Pager: out of bounds"

calcLength :: Integral n => n -> Pager n -> n
calcLength k mm = fromMaybe 0 $ M.lookup k $ imap mm

calcRemove :: Integral n => n -> Pager n -> Pager n
calcRemove k (Pager imap c) = Pager imap' c
	where
		imap' = if k /= min' && k /= max' then M.delete k imap else imap
		min' = fst $ fromJust $ M.lookupMin imap
		max' = fst $ fromJust $ M.lookupMax imap

pagerSize :: Integral n => Pager n -> n
pagerSize = fst . fromJust . M.lookupMax . imap


-- VArrayF interface - required to be freed manually -------------------------------------

data VArrayF a = VArrayF { vArraySize :: GLintptr, vArrayPos :: GLintptr } deriving (Eq,Ord)

newVArrayF :: (HandVBO m, Storable a, Foldable f) => f a -> m (VArrayF a)
newVArrayF xs = newVArrayF' =<<
	(liftIO $ newListArray (0, pred $ length xs) $ foldr (:) [] xs)


newVArrayF' :: (HandVBO m, Storable a) => StorableArray Int a -> m (VArrayF a)
newVArrayF' a = do
	i <- liftIO $ getNumElements a
	let s = itoi $ subSizeOf a * i
	g <- VArrayF s <$> vboAlloc (subSizeOf a) s
	vboUpdate g a
	return g


drawArraysF :: (MonadIO m, Storable a) => [VArrayF a] -> m ()
drawArraysF [] = return ()
drawArraysF gs@(g:_) = let
		f = itoi . (`quot` (subSizeOf g))
		r = map (mapTuple f) $ drawRanges gs
	in mapM_ (uncurry (glDrawArrays GL_TRIANGLES)) r


-- | After using @removeVArrayF@, further calls with the given VArrayF are undefined.
removeVArrayF :: HandVBO m => VArrayF a -> m ()
removeVArrayF (VArrayF _ i) = updatePager $ return . (,()) . calcRemove i


-- VArray --------------------------------------------------------------------------------

newtype VArray a = VArray (Weak (MVar (VArrayF a)))

newVArray :: (HandVBO m, Storable a, Foldable f) => f a -> m (VArray a)
newVArray xs = do
	mvbo <- getVBOMVar
	f <- newVArrayF xs
	mva <- liftIO $ newMVar f >>= \a -> mkWeakMVar a (putStrLn "fin" >> free mvbo f)
	return $ VArray mva
	where
		free mvbo f = modifyMVarMasked_ mvbo $ \vbo -> runHandVBOT' vbo $ removeVArrayF f

drawArrays :: (MonadIO m, Storable a) => [VArray a] -> m ()
drawArrays xs = do
	-- ys <- liftIO $ sequence $ catMaybes $
	ys <- liftIO $ fmap catMaybes $ sequence $ for xs $ \(VArray x) -> deRefWeak x
	mapM (liftIO . readMVar) ys >>= drawArraysF



-- ~ fmap2 = fmap . fmap

-- ~ whenMaybe :: Maybe a -> (a -> m ()) -> m ()
-- ~ whenMaybe m f = case m of
	-- ~ Just a -> f a
	-- ~ Nothing -> return ()


-- GL extension for VAO ------------------------------------------------------------------

glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = glBindVertexArrayOES


frame :: HandVBO m => m (VArray (V3 Float))
frame = newVArray $
  [ (V3 1 1 0), (V3 1 (-1) 0), (V3 (-1) (-1) 0)
  , (V3 (-1) (-1) 0), (V3 (-1) 1 0), (V3 1 1 0)
  ]
