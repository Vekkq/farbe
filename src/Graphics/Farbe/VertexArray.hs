{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.VertexArray where

import Graphics.Farbe.Vec
import Graphics.Farbe.Utility

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Foreign hiding (void)
import Foreign.C

import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

import System.Mem
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Control.Monad.State.Lazy




data VBOState = VBOState
	{ pager :: Pager GLintptr
	, vboIndex :: GLuint
	}

class MonadIO m => HandVBO m where
	stateVBO :: (VBOState -> (a, VBOState)) -> m a
	getVBOMVar :: m (MVar VBOState)

getVBO :: HandVBO m => m VBOState
getVBO = stateVBO (\s -> (s, s))

setVBO :: HandVBO m => VBOState -> m ()
setVBO s = stateVBO (\_ -> ((), s))


instance MonadIO m => HandVBO (StateT VBOState m) where
	stateVBO = state
	getVBOMVar = error "no MVar for StateT"

-- VBO manager ---------------------------------------------------------------------------

initHandVBOState :: MonadIO m => GLintptr -> m VBOState
initHandVBOState s = liftIO $ do
	vboMan <- withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER vboMan
	glBufferData GL_ARRAY_BUFFER s nullPtr GL_DYNAMIC_DRAW
	return $ VBOState (newPager s) vboMan

vboUpdate :: (MonadIO m, Storable a) => VArrayF a -> StorableArray Int a -> m ()
vboUpdate (VArrayF s i) a =
	liftIO $ withStorableArray a $ \p -> glBufferSubData GL_ARRAY_BUFFER i s $ castPtr p


vboAlloc :: HandVBO m => GLintptr -> GLintptr -> m GLintptr
vboAlloc a i = do
	pager <- pager <$> getVBO
	let maybeP = calcAlloc a pager i
	case maybeP of
		Just (pager', p) -> do
			putPager pager'
			return p
		Nothing -> do
			liftIO $ putStrLn $
				"The VBO manager exceeded its allocated size. Recovering."
				++ "Consider to increase its default. "
			vboRecover
			vboAlloc a i

vboRecover :: HandVBO m => m ()
vboRecover = do
	pager <- pager <$> getVBO
	let size = fst $ M.findMax $ imap pager
	let newSize = size*2
	oldvbo <- vboIndex <$> getVBO
	p <- liftIO $ withPtr_ $ glGetBufferPointervOES GL_ARRAY_BUFFER GL_BUFFER_MAP_POINTER_OES
	v <- liftIO $ withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER v
	glBufferData GL_ARRAY_BUFFER newSize p GL_STATIC_DRAW
	deleteBuffer oldvbo
	-- ~ mvm <- getVBO
	let pager' = pager { imap = fixKey size newSize $ imap pager }
	setVBO $ VBOState pager' v
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
	vm <- getVBO
	(p',r) <- f $ pager vm
	setVBO $ vm { pager = p' }
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


-- VArrayF interface - currently required to be freed manually ---------------------------

data VArrayF a = VArrayF { vArraySize :: GLintptr, vArrayPos :: GLintptr } deriving (Eq,Ord,Show)

newVArrayF :: (HandVBO m, Storable a, Foldable f) => f a -> m (VArrayF a)
newVArrayF xs = newVArrayF' =<<
	(liftIO $ newListArray (0, pred $ length xs) $ foldr (:) [] xs)


newVArrayF' :: (HandVBO m, Storable a) => StorableArray Int a -> m (VArrayF a)
newVArrayF' a = do
	i <- liftIO $ getNumElements a
	let s = itoi $ subSizeOf a * i
	g <- VArrayF s <$> vboAlloc (subSizeOf a) s
	vboUpdate g a
	return $ g


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

newtype VArray a = VArray { unVArray :: (MVar (VArrayF a)) }

newVArray :: (HandVBO m, Storable a, Foldable f) => f a -> m (VArray a)
newVArray xs = do
	va <- newVArrayF xs
	mva <- liftIO $ newMVar va
	mvbo <- getVBOMVar
	liftIO $ mkWeakMVar mva $ catchMVarBlocked 6 $
		modifyMVar_ mvbo $ execStateT (removeVArrayF va)
	return $ VArray mva


drawArrays :: (MonadIO m, Storable a) => [VArray a] -> m ()
drawArrays xs = do
	ys <- liftIO $ mapM (readMVar . unVArray) xs
	drawArraysF ys


-- GL extension for VAO ------------------------------------------------------------------

glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = glBindVertexArrayOES


frame :: [V3 Float]
frame =
  [ (V3 1 1 0), (V3 1 (-1) 0), (V3 (-1) (-1) 0)
  , (V3 (-1) (-1) 0), (V3 (-1) 1 0), (V3 1 1 0)
  ]

-- ~ frame :: HandVBO m => m (VArray (V3 Float))
-- ~ frame = newVArray $
  -- ~ [ (V3 1 1 0), (V3 1 (-1) 0), (V3 (-1) (-1) 0)
  -- ~ , (V3 (-1) (-1) 0), (V3 (-1) 1 0), (V3 1 1 0)
  -- ~ ]
