{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}

module Graphics.Farbe where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Window


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Char
import Data.List
import Data.Ord (comparing)
import Data.Function
import Data.Foldable
import Data.Array.Storable
import Data.Array.Base
import Numeric
import Foreign hiding (void)
import Foreign.C

-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Types

import Control.Exception
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

-- ~ import Data.Typeable

import Debug.Trace



-- GL Monad ------------------------------------------------------------------------------

data GLState = GLState
	{ glConfig :: GLConfig
	, counter :: MVar Int
	, vbo :: MVar VBOMan
	-- ~ , glWork :: MVar [(GLint, IO ())]
	-- ~ , resource :: MVar (M.IntMap GLint) -- do i need that?
	-- ~ , glLog :: MVar [String] -- make a new monad transformer for log
	-- add hook that will receive render status
	}

newtype GL m a = GL { unGL :: ReaderT GLState m a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadWriter w, MonadState s, MonadError e, MonadIO
		, MonadFix, MonadPlus, MonadWindow
		)

instance MonadTrans GL where
	lift = GL . lift

instance MonadReader r m => MonadReader r (GL m) where
	ask = lift $ ask
	local f = withw $ mapReaderT (local f)
		where
		withw g = GL . g . unGL

instance Monad m => Semigroup (GL m a) where
	(<>) = (>>)

instance Monad m => Monoid (GL m a) where
	mempty = return $ error ""


class MonadIO m => MonadGL m where
	glState :: m GLState

count :: MonadGL m => m Int
count = do
		c <- counter <$> glState
		liftIO $ modifyMVar c (\i -> return (succ i, i))

instance MonadIO m => MonadGL (GL m) where
	glState = GL ask

instance MonadGL m => MonadGL (ReaderT r m) where
	glState = lift glState


instance MonadGL m => MonadGL (StateT s m) where
	glState = lift glState

instance (MonadGL m, Monoid w) => MonadGL (WriterT w m) where
	glState = lift glState

instance (MonadGL m, Monoid w) => MonadGL (RWST r w s m) where
	glState = lift glState

instance MonadGL m => MonadGL (ContT c m) where
	glState = lift glState

instance MonadGL m => MonadGL (ExceptT e m) where
	glState = lift glState


runGL :: MonadIO m => GLConfig -> GL m a -> m a
runGL conf (GL m) = do
	vbom <- liftIO $ initVBOMan (glVBOSize conf) >>= newMVar
	counter <- liftIO $ newMVar 0
	runReaderT m $ GLState conf counter vbom

data GLConfig = GLConfig { glVBOSize :: GLintptr }

glDefaultConfig = GLConfig { glVBOSize = (2^24) }



-- Tasks ---------------------------------------------------------------------------------

newtype PostShaderProgramM m a = PostShaderProgramM { unpsp :: WriterT [(String, PreRenderM (GL IO) ())] m a }
	deriving
		(Functor, Applicative, Monad, MonadTrans, MonadIO, MonadGL)

instance PreRender m => PreRender (PostShaderProgramM m) where
	preRender = lift . preRender

class Monad m => PostShaderProgram m where
	postShaderProgramList :: [(String, PreRenderM (GL IO) ())] -> m ()

instance Monad m => PostShaderProgram (PostShaderProgramM m) where
	postShaderProgramList = PostShaderProgramM . tell

postShaderProgram :: PostShaderProgram m => String -> PreRenderM (GL IO) () -> m ()
postShaderProgram s a = postShaderProgramList [(s,a)]

runPostShaderProgram :: (MonadGL m, PreRender m) => PostShaderProgramM m a -> m a
runPostShaderProgram p = do
	(a,w) <- runWriterT $ unpsp p
	let b = sequence $ map snd $ nubBy ((==) `on` fst) w
	preRender =<< (liftGL $ snd <$> collectPreRender b)
	return a

liftGL :: (MonadGL m, MonadIO m) => GL IO a -> m a
liftGL gl = do
	r <- glState
	liftIO $ runReaderT (unGL gl) r


newtype PreRenderM m a = PreRenderM { unprp :: WriterT (GL IO ()) m a }
	deriving
		(Functor, Applicative, Monad, MonadTrans, MonadIO, MonadGL)


class Monad m => PreRender m where
	preRender :: GL IO () -> m ()

instance Monad m => PreRender (PreRenderM m) where
	preRender :: GL IO () -> PreRenderM m ()
	preRender = PreRenderM . tell

instance PostShaderProgram m => PostShaderProgram (PreRenderM m) where
	postShaderProgramList = lift . postShaderProgramList

collectPreRender :: MonadGL m => PreRenderM m a -> m (a, m ())
collectPreRender p = fmap2 liftGL $ runWriterT $ unprp p

fmap2 f = fmap (fmap f)

(.:) = (.).(.)



-- Shader building monad -----------------------------------------------------------------

data BuildShaderState = BuildShaderState
	{ shaderId :: Shader
	, header :: S.Set String
	, cExpr :: [String]
	}

emptyShaderState :: Shader -> BuildShaderState
emptyShaderState i = BuildShaderState i S.empty []

newtype BuildShaderM m r = BuildShaderM { unBuildShaderM :: StateT BuildShaderState m r }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, MonadTrans
		)

runBuildShader :: Shader -> BuildShaderM m a -> m (a, BuildShaderState)
runBuildShader i b = runStateT (unBuildShaderM b) $ emptyShaderState i


instance MonadGL m => MonadGL (BuildShaderM m) where
	glState = lift glState

instance PostShaderProgram m => PostShaderProgram (BuildShaderM m) where
	postShaderProgramList = lift . postShaderProgramList

instance PreRender m => PreRender (BuildShaderM m) where
	preRender = lift . preRender


class (MonadGL m, PostShaderProgram m, PreRender m) => BuildShader m where
	buildShaderState :: (BuildShaderState -> (a, BuildShaderState)) -> m a

instance (MonadGL m, PostShaderProgram m, PreRender m) => BuildShader (BuildShaderM m) where
	buildShaderState = BuildShaderM . state

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m ()
addHeader i a n = buildShaderState $ \s -> ((),) $
		s { header = S.insert (unwords [i, glCNameWithPrec a, n, ";"]) $ header s }

addCExpr :: BuildShader m => String -> String -> m ()
addCExpr n sa = buildShaderState $ \s -> ((),) $
	s { cExpr = (n ++ " = " ++ sa) : cExpr s }

getShader :: BuildShader m => m Shader
getShader = buildShaderState $ \s -> (shaderId s, s)

buildShaderStateGet :: BuildShader m => m BuildShaderState
buildShaderStateGet = buildShaderState $ \s -> (s,s)

buildShaderStatePut :: BuildShader m => BuildShaderState -> m ()
buildShaderStatePut a = buildShaderState $ \_ -> ((),a)



-- Expr ----------------------------------------------------------------------------------

data Ast
	= Val { val :: BuildShaderM (PreRenderM (PostShaderProgramM (GL IO))) String }
	| Fn { fnName :: String, fnAst :: [Ast] }


liftBuildShaderExt
	:: (MonadGL m, BuildShader m, PreRender m, PostShaderProgram m)
	=> BuildShaderM (PreRenderM (PostShaderProgramM (GL IO))) a
	-> m a
liftBuildShaderExt g = do
	b <- buildShaderStateGet
	(((a, b'), pre), post) <-
		liftGL $ runWriterT $ unpsp $ runWriterT $ unprp $ runStateT (unBuildShaderM g) b
	buildShaderStatePut b'
	preRender pre
	postShaderProgramList post
	return a

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.


-- | Expression for shaders.
-- | e states the environment, which is either vertex or fragment shader.
data Expr e a = Expr { ast :: Ast }

-- AttribM -------------------------------------------------------------------------------

newtype AttribM m a = AttribM { unAttrib :: StateT Int m a }
	deriving
		(Functor, Applicative, Monad, MonadTrans, MonadIO)

-- ~ instance MonadState s m => MonadState s (AttribM m) where
	-- ~ get = lift get
	-- ~ put s = lift $ put s

class Monad m => Attrib m where
	offset :: m Int
	advanceBy :: Storable s => s -> m ()

instance Monad m => Attrib (AttribM m) where
	offset = AttribM get
	advanceBy a = AttribM $ modify (sizeOf a +)

instance MonadGL m => MonadGL (AttribM m) where
	glState = lift glState

instance PostShaderProgram m => PostShaderProgram (AttribM m) where
	postShaderProgramList = lift . postShaderProgramList

instance PreRender m => PreRender (AttribM m) where
	preRender = lift . preRender

instance BuildShader m => BuildShader (AttribM m) where
	buildShaderState = lift . buildShaderState

instance (Monad m) => MonadFail m where
		fail = return . error


genName :: Int -> String
genName i = (map (:[]) ['a'..'z'] ++ map show [1..]) !! i

generateName :: (MonadGL m, GLtype a) => a -> m String
generateName a = count >>= return . (glShortName a++) . genName

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f

type Vao = GLuint

-- | Make VAO
setAttributes :: (BuildShader m, AttrType a b) => Shader -> a -> m (Vao, b)
setAttributes s a = do
	i <- glGenVertexArray
	glBindVertexArray i
	e <- evalStateT (unAttrib $ setAttribute s a) 0
	return (i, e)

class Storable a => AttrType a b where
	setAttribute :: (BuildShader m, Attrib m) => Shader -> a -> m b

instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute s _ = liftM2 (,) (setAttribute s (err :: a)) (setAttribute s (err :: b))

instance AttrType (V3 Float) (V3 (Expr V Float)) where
	setAttribute s a = do
		Expr (Val n) <- setupAttribute1 s a
		return $ fromList $ map (\c -> Expr $ Val $ fmap (++c) n) [".x", ".y", ".z"]


setupAttribute1
	:: (GLtype a, BuildShader m, Attrib m)
	=> Shader
	-> a
	-> m (Expr V a)
setupAttribute1 s a = do
	n <- generateName a
	o <- offset
	postShaderProgram n $ withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c
		glVertexAttribPointer p
			(glComponents a)
			(glType a)
			(glNormalized a)
			0
			(intPtrToPtr $ IntPtr o)
		glEnableVertexAttribArray p
	advanceBy a
	initExpr <- makeRunOnce $ do
		addHeader "attribute" a n
		return n
	return $ (Expr $ Val $ runOnce initExpr)

-- Uploadable ----------------------------------------------------------------------------

makeFloat :: MonadGL m => m (Expr e Float, MVar Float)
makeFloat = makeVar

class GLtype a => Uploadable a e | e -> a where
	makeVar :: MonadGL m => m (e, MVar a)


instance Uploadable Float (Expr e Float) where
	makeVar = makeVarDefault "f"

-- ~ makeVarDefault = undefined

makeVarDefault :: forall a m e . (MonadGL m, GLtype a) => String -> m (Expr e a, MVar a)
makeVarDefault c = do
	m <- liftIO $ newMVar glDefault
	vname <- generateName (err :: a)
	let r = do
		s <- getShader
		addHeader "uniform" (err :: a) vname
		postShaderProgram vname $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged glDefault (glUpload l)
			when (l >= 0) $ preRender $ liftIO (tryReadMVar m) >>= maybe (pure ()) (runwc wc)
		return vname
	return (Expr $ Val r, m)

updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ swapMVar m a



liftE :: String -> Expr e1 a1 -> Expr e2 a2
liftE s (Expr a) = Expr $ Fn s [a]

liftE2 :: String -> Expr e1 a1 -> Expr e2 a2 -> Expr e3 a3
liftE2 s (Expr a) (Expr b) = Expr $ Fn s [a,b]

instance Num a => Num (Expr e a) where
	(+) = liftE2 "+"
	(*) = liftE2 "*"
	(-) = liftE2 "-"
	abs = undefined
	signum = undefined
	fromInteger = Expr . Val . return . ($ []) . showFFloat Nothing . fromInteger

instance Fractional a => Fractional (Expr e a) where
	fromRational = Expr . Val . return . ($ []) . showFFloat Nothing . fromRat
	(/) = liftE2 "/"


instance Floating a => Floating (Expr e a) where
	pi = Expr $ Val $ return $ show pi
	exp = liftE "exp"
	log = liftE "log"
	sqrt = liftE "sqrt"
	(**) = liftE2 "^"
	sin = liftE "sin"
	cos = liftE "cos"
	tan = liftE "tan"
	asin = liftE "asin"
	acos = liftE "acos"
	atan = liftE "atan"
	sinh = liftE "sinh"
	cosh = liftE "cosh"
	tanh = liftE "tanh"
	asinh = liftE "asinh"
	acosh = liftE "acosh"
	atanh = liftE "atanh"




compose1 :: BuildShader m => Ast -> m String
compose1 ast = case ast of
	Val s -> liftBuildShaderExt s
	Fn s (p1:p2:[]) | isOp s -> liftM2 (\a b -> par $ a ++ s ++ b)
		(compose1 p1) (compose1 p2)
	Fn "if" (p1:p2:p3:[]) -> liftM3 (\a b c -> par $ a ++ "?" ++ b ++ ":" ++ c)
		(compose1 p1) (compose1 p2) (compose1 p3)
	Fn s as -> (s++) . par . intercalate ", " <$> mapM compose1 as
	where
		isOp :: String -> Bool
		isOp (x:_) = not $ isAlpha x
		isOp [] = False

par :: String -> String
par s = "(" ++ s ++ ")"

compose :: BuildShader m => String -> Expr e r -> m ()
compose s e = (compose1 $ ast e) >>= addCExpr s

class Raster v f where
	raster :: (V4 (Expr V Float), v) -> f


instance GLtype a => Raster (Expr V a) (Expr F a) where
	raster (v,e) = let
		a = err :: a
		vert n = do
			compose "gl_Position" $ vec4 v
			compose n e
			addHeader "varying" a n -- will likely become borked for tuple types
		frag = do
			n <- generateName a
			addHeader "in" a n
			i <- getShader
			addShader i GL_VERTEX_SHADER $ vert n
			return n
		in Expr $ Val frag


vec4 :: V4 (Expr e a) -> Expr e (V4 a)
vec4 v = Expr $ Fn "vec4" $ map ast $ toList v


compile :: forall a b m. (AttrType a b, MonadGL m)
	=> (b -> V4 (Expr F Float))
	-> m ([GArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	let g = addShader sp GL_FRAGMENT_SHADER $ do
		(i,e) <- setAttributes sp (err :: a)
		compose "gl_FragColor" $ vec4 $ f e
		return i
	(vao,exec) <- collectPreRender $ runPostShaderProgram g
	return $ \garrs -> do
		glBindVertexArray vao
		glUseProgram sp
		exec
		drawArrays garrs



-- Ast optimizations ---------------------------------------------------------------------
-- without context probably quickly to break stuff



-- ~ same (x:y:xs) = x == y && same (y:xs)
-- ~ same _ = True

-- ~ findInStringDepth :: Int -> String -> String -> Bool
-- ~ findInStringDepth i s = any (isPrefixOf s) . take i . tails

-- ~ vecRow = [".x", ".y", ".z", ".w"]






-- Shader compilation --------------------------------------------------------------------

type Shader = GLuint


addShader :: MonadGL m => Shader -> GLenum -> BuildShaderM m a -> m a
addShader sp t shdr = do
	(a,st) <- runStateT (unBuildShaderM shdr) $ emptyShaderState sp
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ unlines (map (("  "++) . (++";")) $ reverse $ cExpr st)
		++ "}"
	liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		checkShaderError str i
		-- ~ putStrLn str
		glAttachShader sp i
		when (t == GL_FRAGMENT_SHADER) $ glLinkProgram sp
	return a

checkShaderError :: String -> GLuint -> IO ()
checkShaderError str shdr = bracket (mallocArray $ 2^10) free $ \er ->
	bracket malloc free $ \errLength -> do
		glGetShaderInfoLog shdr (2^10) errLength er
		peekArray0 (CChar 0) er >>= \ce -> case map castCCharToChar ce of
			"" -> return ()
			e -> do
				putStrLn str
				putStrLn e


withPtr :: Storable a => (Ptr a -> IO b) -> IO (a, b)
withPtr f = do
	alloca $ \p -> do
		x <- f p
		y <- peek p
		return (y, x)

withPtr_ :: Storable a => (Ptr a -> IO ()) -> IO a
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


-- VBO manager ---------------------------------------------------------------------------

data VBOMan = VBOMan
	{ pager :: Pager GLintptr
	, vboIndex :: GLuint
	}

initVBOMan :: GLintptr -> IO VBOMan
initVBOMan s = do
	vbo <- withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER vbo
	glBufferData GL_ARRAY_BUFFER s nullPtr GL_STATIC_DRAW
	return $ VBOMan (newPager s) vbo

getVBO :: MonadGL m => m GLuint
getVBO = liftIO . fmap vboIndex . readMVar =<< vbo <$> glState

getPager :: MonadGL m => m (Pager GLintptr)
getPager = liftIO . fmap pager . readMVar =<< vbo <$> glState

updatePager :: MonadGL m => (Pager GLintptr -> m (Pager GLintptr, a)) -> m a
updatePager f = do
	vmm <- vbo <$> glState
	vm <- liftIO $ takeMVar vmm
	(p',r) <- f $ pager vm
	liftIO $ putMVar vmm $ vm { pager = p' }
	return r

putPager :: MonadGL m => Pager GLintptr -> m ()
putPager a = updatePager $ \_ -> return (a, ())

vboUpdate :: (MonadGL m, Storable a) => GArray a -> StorableArray Int a -> m ()
vboUpdate (GArray s i) a =
	liftIO $ withStorableArray a $ \p -> glBufferSubData GL_ARRAY_BUFFER i s $ castPtr p


vboAlloc :: MonadGL m => GLintptr -> GLintptr -> m GLintptr
vboAlloc a i = do
	pager <- getPager
	let maybeP = calcAlloc a pager i
	case maybeP of
		Just (pager,p) -> do
			putPager pager
			return p
		Nothing -> do
			liftIO $ putStrLn $
				"The vbo exceeded its allocated size. "
				++ "Consider to increase its default. "
			vboRecover
			vboAlloc a i

vboRecover :: MonadGL m => m ()
vboRecover = do
	pager <- getPager
	let size = fst $ M.findMax $ imap pager
	let newSize = size*2
	oldvbo <- getVBO
	p <- liftIO $ withPtr_ $ glGetBufferPointervOES GL_ARRAY_BUFFER GL_BUFFER_MAP_POINTER_OES
	v <- liftIO $ withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER v
	glBufferData GL_ARRAY_BUFFER newSize p GL_STATIC_DRAW
	glDeleteBuffer oldvbo
	mvm <- vbo <$> glState
	let pager' = pager { imap = fixKey size newSize $ imap pager }
	liftIO $ swapMVar mvm $ VBOMan pager' v
	return ()
	where
		fixKey o n m = M.insert n (negate n) $ M.delete o m

vboFree :: MonadGL m => GLintptr -> m ()
vboFree a = updatePager $ \p -> return $ (,()) $ calcRemove a p

glDeleteBuffer :: MonadIO m => GLuint -> m ()
glDeleteBuffer i = liftIO $ alloca $ \p -> do
	poke p i
	glDeleteBuffers 1 p

-- | Merge neighboring ranges
condense :: (Eq n, Num n) => [(n,n)] -> [(n,n)]
condense (x@(xp,xr):y@(yp,yr):xs)
	| xp+xr == yp = condense $ (xp, xr+yr):xs
	| otherwise = x : condense (y:xs)
condense (x:_) = [x]
condense [] = []

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x,y) = (f x, f y)

gArrayRange :: GArray a -> (CPtrdiff, CPtrdiff)
gArrayRange (GArray s p) = (p,s)

drawRanges :: [GArray a] -> [(CPtrdiff, CPtrdiff)]
drawRanges = condense . map gArrayRange . sortBy (comparing gArrayPos)


-- GArray interface ----------------------------------------------------------------------

data GArray a = GArray { gArraySize :: GLintptr, gArrayPos :: GLintptr } deriving (Eq,Ord)

newGArray :: (MonadGL m, Storable a, Foldable f) => f a -> m (GArray a)
newGArray xs = newGArray' =<<
	(liftIO $ newListArray (0, pred $ length xs) $ foldr (:) [] xs)


newGArray' :: (MonadGL m, Storable a) => StorableArray Int a -> m (GArray a)
newGArray' a = do
	i <- liftIO $ getNumElements a
	let s = itoi $ subSizeOf a * i
	g <- GArray s <$> vboAlloc (subSizeOf a) s
	vboUpdate g a
	return g


drawArrays :: (MonadIO m, Storable a) => [GArray a] -> m ()
drawArrays [] = return ()
drawArrays gs@(g:_) = let
		f = itoi . (`quot` (subSizeOf g))
		r = map (mapTuple f) $ drawRanges gs
	in mapM_ (uncurry (glDrawArrays GL_TRIANGLES)) r


-- | After using @removeGArray@, further calls with the given GArray are undefined.
removeGArray :: MonadGL m => GArray a -> m ()
removeGArray (GArray _ i) = updatePager $ return . (,()) . calcRemove i



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


-- RunWhenChanged ------------------------------------------------------------------------

data RunWhenChanged m a = RunWhenChanged (a -> m ()) (MVar a)

makeRunWhenChanged :: MonadIO m => a -> (a -> m2 ()) -> m (RunWhenChanged m2 a)
makeRunWhenChanged a m = liftIO $ RunWhenChanged m <$> newMVar a

runwc :: (MonadIO m, Eq a) => RunWhenChanged m a -> a -> m ()
runwc (RunWhenChanged f ml) a = do
	l <- liftIO $ readMVar ml
	if a == l
		then return ()
		else do
			liftIO $ swapMVar ml a
			f a

-- GL extension for VAO ------------------------------------------------------------------

glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = liftIO . glBindVertexArrayOES




-- GL type information -------------------------------------------------------------------

class (Eq a, Storable a) => GLtype a where
	glCName :: a -> String
	glType :: a -> GLenum
	glComponents :: a -> GLint
	glComponents _ = 1
	glNormalized :: a -> GLboolean
	glNormalized _ = GL_FALSE
	glShortName :: a -> String
	glShortName a = take 1 $ glCName a
	glUpload :: MonadIO m => GLint -> a -> m ()
	glDefault :: a
	glPrecision :: a -> String
	glPrecision _ = "highp"
	glCNameWithPrec :: a -> String
	glCNameWithPrec a = glPrecision a ++ " " ++ glCName a


instance GLtype Bool where
	glCName _ = "bool"
	glType _ = GL_BOOL
	glUpload i b = glUniform1i i $ if b then 1 else 0
	glDefault = False

instance GLtype Int32 where
	glCName _ = "int"
	glType _ = GL_INT
	glUpload = glUniform1i
	glDefault = 0

instance GLtype Float where
	glCName _ = "float"
	glType _ = GL_FLOAT
	glUpload = glUniform1f
	glDefault = 0

instance GLtype (V2 Float) where
	glCName _ = "vec2"
	glType _ = GL_FLOAT
	glComponents _ = 2
	glUpload i (V2 a b) = glUniform2f i a b
	glDefault = V2 0 0

instance GLtype (V3 Float) where
	glCName _ = "vec3"
	glType _ = GL_FLOAT
	glComponents _ = 3
	glUpload i (V3 a b c) = glUniform3f i a b c
	glDefault = V3 0 0 0

instance GLtype (V4 Float) where
	glCName _ = "vec4"
	glType _ = GL_FLOAT
	glComponents _ = 4
	glUpload i (V4 a b c d) = glUniform4f i a b c d
	glDefault = V4 0 0 0 0


instance GLtype (V2 Int32) where
	glCName _ = "vec2"
	glType _ = GL_INT
	glComponents _ = 2
	glUpload i (V2 a b) = glUniform2i i a b
	glDefault = V2 0 0

instance GLtype (V3 Int32) where
	glCName _ = "vec3"
	glType _ = GL_INT
	glComponents _ = 3
	glUpload i (V3 a b c) = glUniform3i i a b c
	glDefault = V3 0 0 0

instance GLtype (V4 Int32) where
	glCName _ = "vec4"
	glType _ = GL_INT
	glComponents _ = 4
	glUpload i (V4 a b c d) = glUniform4i i a b c d
	glDefault = V4 0 0 0 0

