{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Farbe where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Window


import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.List
import Data.Ord (comparing)
import Data.Foldable
import Data.Array.Storable
import Data.Array.Base
import Numeric
import Foreign hiding (void)
import Foreign.C

-- ~ import Graphics.GL
import Graphics.GL.Embedded20
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Types

import Control.Exception
import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (WriterT, MonadWriter)
import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

-- ~ import Data.Typeable

-- ~ import Debug.Trace



-- GL Monad ------------------------------------------------------------------------------

data GLState = GLState
	{ counter :: MVar Int
	, vbo :: MVar VBOMan
	-- ~ , glWork :: MVar [(GLint, IO ())]
	-- ~ , resource :: MVar (M.IntMap GLint) -- do i need that?
	-- ~ , glLog :: MVar [String] -- make a new monad transformer for log
	-- add hook that will receive render status
	}


runGL :: MonadIO m => GL m a -> m a
runGL (GL m) = do
	vbom <- liftIO $ initVBOMan (2^24-1) >>= newMVar
	counter <- liftIO $ newMVar 0
	runReaderT m $ GLState counter vbom



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

class MonadIO m => MonadGL m where
	glState :: m GLState
	count :: m Int

instance MonadIO m => MonadGL (GL m) where
	glState = GL ask
	count = do
		c <- counter <$> glState
		liftIO $ modifyMVar c (\i -> return (succ i, i))


liftGL :: (MonadGL m, MonadIO m) => GL IO a -> m a
liftGL gl = do
	r <- glState
	liftIO $ runReaderT (unGL gl) r

liftBuildShaderGL :: MonadGL m => BuildShader (GL IO) a -> BuildShader m a
liftBuildShaderGL g = do
	s <- get
	(a,s') <- liftGL $ runStateT (unBuildShader g) s
	put s'
 	return a


instance MonadGL m => MonadGL (ReaderT r m) where
	glState = lift glState
	count = lift count


instance MonadGL m => MonadGL (StateT s m) where
	glState = lift glState
	count = lift count

instance (MonadGL m, Monoid w) => MonadGL (WriterT w m) where
	glState = lift glState
	count = lift count

instance (MonadGL m, Monoid w) => MonadGL (RWST r w s m) where
	glState = lift glState
	count = lift count

instance MonadGL m => MonadGL (ContT c m) where
	glState = lift glState
	count = lift count

instance MonadGL m => MonadGL (ExceptT e m) where
	glState = lift glState
	count = lift count



-- Shader compilation --------------------------------------------------------------------

type VaoId = GLuint
type ShaderId = GLuint


addShader :: MonadGL m => ShaderId -> GLenum -> BuildShader m () -> m (m (), m ())
addShader sp t shdr = do
	-- ~ (BuildShaderState _ hs es io) <- execStateT (unBuildShader shdr) $ emptyShaderState sp
	st <- execStateT (unBuildShader shdr) $ emptyShaderState sp
	let str
		=  "#version 100\n"
		++ unlines (reverse $ header st)
		++ "\n\nvoid main(){\n"
		++ unlines (map (("  "++) . (++";")) $ reverse $ cExpr st)
		++ "}"
	liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		checkShaderError str i
		putStrLn str
		glAttachShader sp i
		when (t == GL_FRAGMENT_SHADER) $ glLinkProgram sp
	return $ (liftGL $ execOnUse st, liftGL $ postBuild st)

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



-- Buffer allocation calculations --------------------------------------------------------

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

getMan :: MonadGL m => m (Pager GLintptr)
getMan = liftIO . fmap pager . readMVar =<< vbo <$> glState

updateMan :: MonadGL m => (Pager GLintptr -> IO (Pager GLintptr, a)) -> m a
updateMan f = liftIO . (flip modifyMVar foo) =<< vbo <$> glState
	where
		foo (VBOMan a v) = do
			(a',x) <- f a
			return (VBOMan a' v, x)

vboAlloc :: MonadGL m => GLintptr -> GLintptr -> m GLintptr
vboAlloc a i = updateMan $ \mn -> return $ fromMaybe e $ calcAlloc a mn i
	where e = error "VBOMan: total size limit reached"
	-- a buffer may be extendable afterall, when copying out all data
	-- and adding it with a bigger call to glbufferdata again.
	-- this requires a higher opengl es version.

vboUpdate :: (MonadGL m, Storable a) => GArray a -> StorableArray Int a -> m ()
vboUpdate (GArray s i) a =
	liftIO $ withStorableArray a $ \p -> glBufferSubData GL_ARRAY_BUFFER i s $ castPtr p


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

-- TODO: have GArray clear itself up on losing reference
-- move garray content into a ioref and add finalizer

data GArray a = GArray { gArraySize :: GLintptr, gArrayPos :: GLintptr } deriving (Eq,Ord)

newGArray :: (MonadGL m, Storable a, Foldable f) => f a -> m (GArray a)
newGArray xs = (liftIO $ newListArray (0, pred $ length xs) $ foldr (:) [] xs)
	>>= newGArray'

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
removeGArray (GArray _ i) = updateMan $ return . (,()) . calcRemove i




-- RunOnce -------------------------------------------------------------------------------

data RunOnce m a = RunOnce (m a) (MVar a)

makeRunOnce :: (MonadIO m) => m2 a -> m (RunOnce m2 a)
makeRunOnce ma = liftIO $ RunOnce ma <$> newEmptyMVar

runOnce :: MonadIO m => RunOnce m a -> m a
runOnce (RunOnce m ma) = do
	maybea <- liftIO $ tryReadMVar ma
	case maybea of
		Just a -> return a
		Nothing -> do
			a <- m
			liftIO $ putMVar ma a
			return a


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
	glPrecision _ = "mediump"
	glCNameWithPrec :: a -> String
	glCNameWithPrec a = glPrecision a ++ " " ++ glCName a


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


-- VAO -----------------------------------------------------------------------------------

newtype VaoM m a = VaoM { unVao :: StateT Int (StateT (GL IO ()) m) a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadFix, MonadPlus, MonadWindow
		)

instance MonadTrans VaoM where
	lift = VaoM . lift . lift

instance MonadState s m => MonadState s (VaoM m) where
	get = lift get
	put s = lift $ put s

class Monad m => Vao m where
	offset :: m Int
	advanceBy :: Storable s => s -> m ()
	posthoc :: GL IO () -> m ()

instance Monad m => Vao (VaoM m) where
	offset = VaoM get
	advanceBy a = VaoM $ modify (sizeOf a +)
	posthoc io = VaoM $ lift $ modify (>>io)

instance Monad m => MonadFail (VaoM m) where
		fail = return . error

instance MonadGL m => MonadGL (VaoM m) where
	glState = lift glState
	count = lift count


genName :: Int -> String
genName i = (map (:[]) ['a'..'z'] ++ map show [1..]) !! i

generateName :: MonadGL m => String -> m String
generateName t = count >>= return . (t++) . genName

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f

-- | Set a VAO. Since VAO setup has to run after shader setup, it wraps it.
setAttributes :: (MonadGL m, AttrType a b) => ShaderId -> a -> (b -> m c) -> m (VaoId, c)
setAttributes s a f = do
	i <- glGenVertexArray
	glBindVertexArray i -- needed?
	-- ~ getVBO >>= liftIO . glBindBuffer GL_ARRAY_BUFFER -- needed?
	(b, m) <- runStateT (evalStateT (unVao $ setAttribute s a) 0) (return ())
	r <- f b
	liftGL m
	return (i, r)

class Storable a => AttrType a b where
	setAttribute :: MonadGL m => ShaderId -> a -> VaoM m b

instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute s _ = liftM2 (,) (setAttribute s (err :: a)) (setAttribute s (err :: b))

instance AttrType (V3 Float) (V3 (Expr V Float)) where
	setAttribute s a = do
		Expr (Val n) <- setupAttribute1 s a
		return $ fromList $ map (\c -> Expr $ Val $ fmap (++c) n) [".x", ".y", ".z"]


setupAttribute1
	:: (GLtype a, MonadGL m)
	=> ShaderId
	-> a
	-> VaoM m (Expr V a)
setupAttribute1 s a = do
	n <- generateName $ "v" ++ glShortName a
	o <- offset
	posthoc $ withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c -- TODO: has to run after compiler linking
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

-- Expr ----------------------------------------------------------------------------------

data Ast
	= Val { val :: BuildShader (GL IO) String }
	| Fn { fnName :: String, fnAst :: [Ast] }


data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.


-- | Expression for shaders.
-- | e states the environment, which is either vertex or fragment shader.
data Expr e a = Expr { ast :: Ast }


-- Uploadable ----------------------------------------------------------------------------

makeFloat :: MonadGL m => m (Expr e Float, MVar Float)
makeFloat = makeVar

class GLtype a => Uploadable a e | e -> a where
	makeVar :: MonadGL m => m (e, MVar a)


instance Uploadable Float (Expr e Float) where
	makeVar = makeVarDefault "f"


makeVarDefault :: forall a m e . (MonadGL m, GLtype a) => String -> m (Expr e a, MVar a)
makeVarDefault c = do
	m <- liftIO $ newMVar glDefault
	vname <- generateName c
	r <- makeRunOnce $ do
		s <- getShaderId
		addHeader "uniform" (err :: a) vname
		postBuild $ liftIO $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged glDefault (glUpload l)
			when (l >= 0) $ addExec $ liftIO (tryReadMVar m) >>= maybe (pure ()) (runwc wc)
		return vname
	return (Expr $ Val $ runOnce r, m)

updateMVar :: MonadIO m => MVar a -> a -> m ()
updateMVar m a = liftIO $ void $ swapMVar m a


-- Shader building monad -----------------------------------------------------------------

data BuildShaderState = BuildShaderState
	{ shaderId :: ShaderId
	, header :: [String]
	, cExpr :: [String]
	, postBuild :: GL IO ()
	, execOnUse :: GL IO ()
	}


emptyShaderState :: ShaderId -> BuildShaderState
emptyShaderState i = BuildShaderState i [] [] (pure ()) (pure ())

blankShader :: Monad m => BuildShader m r -> m r
blankShader s = evalStateT (unBuildShader s) $ emptyShaderState 0


newtype BuildShader m r = BuildShader { unBuildShader :: StateT BuildShaderState m r }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, MonadState BuildShaderState, MonadTrans
		)

instance MonadGL m => MonadGL (BuildShader m) where
	glState = lift glState
	count = lift count

addCExpr :: Monad m => String -> String -> BuildShader m ()
addCExpr n sa = modify $ \s -> s { cExpr = (n ++ " = " ++ sa) : cExpr s }

addHeader :: forall a m. (GLtype a, MonadGL m) => String -> a -> String -> BuildShader m ()
addHeader i a n =
	modify $ \s -> s { header = unwords [i, glCNameWithPrec a, n, ";"] : header s }

addPostBuild :: MonadGL m => GL IO () -> BuildShader m ()
addPostBuild io = modify $ \s -> s { postBuild = postBuild s >> io }

addExec :: MonadGL m => GL IO () -> BuildShader m ()
addExec io = modify $ \s -> s { execOnUse = execOnUse s >> io }

getShaderId :: MonadState BuildShaderState m => m ShaderId
getShaderId = shaderId <$> get

liftE' :: String -> Expr e1 a1 -> Expr e2 a2
liftE' s (Expr a) = Expr $ Fn s [a]

liftE2 :: (Ast -> Ast -> Ast) -> Expr e1 a1 -> Expr e2 a2 -> Expr e3 a3
liftE2 f (Expr a) (Expr b) = Expr $ f a b

instance Num a => Num (Expr e a) where
	(+) = liftE2 (\a b -> Fn "+" [a,b])
	(*) = liftE2 (\a b -> Fn "*" [a,b])
	(-) = liftE2 (\a b -> Fn "-" [a,b])
	abs = undefined
	signum = undefined
	fromInteger = Expr . Val . return . ($ []) . showFFloat Nothing . fromInteger

instance Fractional a => Fractional (Expr e a) where
	fromRational = Expr . Val . return . ($ []) . showFFloat Nothing . fromRat
	(/) = liftE2 (\a b -> Fn "/" [a,b])


instance Floating a => Floating (Expr e a) where
	pi = Expr $ Val $ return $ show pi
	exp = liftE' "exp"
	log = liftE' "log"
	sqrt = liftE' "sqrt"
	(**) = liftE2 (\a b -> Fn "^" [a,b])
	sin = liftE' "sin"
	cos = liftE' "cos"
	tan = liftE' "tan"
	asin = liftE' "asin"
	acos = liftE' "acos"
	atan = liftE' "atan"
	sinh = liftE' "sinh"
	cosh = liftE' "cosh"
	tanh = liftE' "tanh"
	asinh = liftE' "asinh"
	acosh = liftE' "acosh"
	atanh = liftE' "atanh"




compose1 :: (MonadGL m, MonadIO m) => Ast -> BuildShader m String
compose1 ast = case ast of
	Val s -> liftBuildShaderGL s
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

compose :: MonadGL m => String -> Expr e r -> BuildShader m ()
compose s e = (compose1 $ ast e) >>= addCExpr s

class Raster v f where
	raster :: (V4 (Expr V Float), v) -> f


instance GLtype a => Raster (Expr V a) (Expr F a) where
	raster (v,e) = let
		a = err :: a
		vert n = do
			compose "gl_Position" $ vec4 v
			compose n e
			addHeader "varying" a n -- borked for tuple types
		frag = do
			n <- generateName $ glShortName a
			addHeader "in " a n
			i <- getShaderId
			(p,exec) <- lift $ addShader i GL_VERTEX_SHADER $ vert n
			addPostBuild p
			addExec exec
			return n
		in Expr $ Val frag


vec4 :: V4 (Expr e a) -> Expr e (V4 a)
vec4 v = Expr $ Fn "vec4" $ map ast $ toList v


compile :: forall a b m. (AttrType a b, MonadGL m)
	=> (b -> V4 (Expr F Float))
	-> m ([GArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(i, exec) <- setAttributes sp (err :: a) $ \e -> do
		(p,exec) <- addShader sp GL_FRAGMENT_SHADER $ compose "gl_FragColor" $ vec4 $ f e
		p
		return exec
	return $ \garrs -> do
		glBindVertexArray i
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



