{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Graphics.Farbe where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Window
import Graphics.Farbe.Utils


import qualified Data.Map as M
import qualified Data.Set as S
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

import GHC.TypeNats
import Data.Proxy

-- ~ import Data.Typeable

-- ~ import Debug.Trace



-- GL Monad ------------------------------------------------------------------------------

data GLState = GLState
	{ glConfig :: GLConfig
	, counter :: MVar Int
	, vboMan :: MVar VBOMan
	-- ~ , glWork :: MVar [(GLint, IO ())]
	-- ~ , resource :: MVar (M.IntMap GLint) -- do i need that?
	-- ~ , glLog :: MVar [String] -- make a new monad transformer for log
	-- add hook that will receive render status
	}

newtype GL m a = GL { unGL :: ReaderT GLState m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadWriter w, MonadState s, MonadError e, MonadIO
		, MonadFix, MonadPlus, MonadWindow
		)

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

instance MonadIO m => MonadGL (GL m) where
	glState = GL ask

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(glState,MonadGL,)

count :: MonadGL m => m Int
count = do
		c <- counter <$> glState
		liftIO $ modifyMVar c (\i -> return (succ i, i))

runGL :: MonadIO m => GLConfig -> GL m a -> m a
runGL conf (GL m) = do
	vbom <- liftIO $ initVBOMan (glVBOSize conf) >>= newMVar
	counter <- liftIO $ newMVar 0
	runReaderT m $ GLState conf counter vbom

data GLConfig = GLConfig
	{ glVBOSize :: GLintptr
	, glDebug :: Bool }

glDefaultConfig :: GLConfig
glDefaultConfig = GLConfig { glVBOSize = (2^24), glDebug = True }

debug :: MonadGL m => m a -> m ()
debug io = do
	d <- glDebug <$> glConfig <$> glState
	when d $ void io

-- Name generation -----------------------------------------------------------------------

genName :: Int -> String
genName i = (map (:[]) ['a'..'z'] ++ map show [1..]) !! i

generateName :: (MonadGL m, GLtype a) => a -> m String
generateName a = count >>= return . (glShortName a++) . genName

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f

-- Tasks ---------------------------------------------------------------------------------

newtype PostShaderProgramM m a = PostShaderProgramM { unpsp :: WriterT [(String, PreRenderM (GL IO) ())] m a }
	deriving
		(Functor, Applicative, Monad, MonadTrans, Alternative, MonadIO, MonadGL, PreRender)

class Monad m => PostShaderProgram m where
	postShaderProgramList :: [(String, PreRenderM (GL IO) ())] -> m ()

instance Monad m => PostShaderProgram (PostShaderProgramM m) where
	postShaderProgramList = PostShaderProgramM . tell

SIMPLEFUNCTION_CLASSINSTANCES(postShaderProgramList,PostShaderProgram,.)

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
		(Functor, Applicative, Monad, MonadTrans, MonadIO, MonadGL, PostShaderProgram)

class Monad m => PreRender m where
	preRender :: GL IO () -> m ()

instance Monad m => PreRender (PreRenderM m) where
	preRender = PreRenderM . tell

SIMPLEFUNCTION_CLASSINSTANCES(preRender,PreRender,.)

collectPreRender :: MonadGL m => PreRenderM m a -> m (a, m ())
collectPreRender p = fmap2 liftGL $ runWriterT $ unprp p

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 f = fmap (fmap f)

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
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
		, MonadGL, PostShaderProgram, PreRender
		)

runBuildShader :: Shader -> BuildShaderM m a -> m (a, BuildShaderState)
runBuildShader i b = runStateT (unBuildShaderM b) $ emptyShaderState i


class (MonadGL m, PostShaderProgram m, PreRender m) => BuildShader m where
	buildShaderState :: (BuildShaderState -> (a, BuildShaderState)) -> m a

instance (MonadGL m, PostShaderProgram m, PreRender m) => BuildShader (BuildShaderM m) where
	buildShaderState = BuildShaderM . state

SIMPLEFUNCTION_CLASSINSTANCES(buildShaderState,BuildShader,.)

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
	= Val { val :: AstM String }
-- propose: Val { val :: Int, type :: Enum, astM :: astM () }
	| Fn { fnName :: String, fnAst :: [Ast] }

type AstM = BuildShaderM (PostShaderProgramM (PreRenderM (GL IO)))

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

-- | Expression for shaders.
-- | e states the environment, which is either vertex or fragment shader.
data Expr e a = Expr { ast :: Ast } deriving (Functor)

liftBuildShaderExt
	:: (MonadGL m, BuildShader m, PreRender m, PostShaderProgram m)
	=> BuildShaderM (PostShaderProgramM (PreRenderM (GL IO))) a
	-> m a
liftBuildShaderExt g = do
	b <- buildShaderStateGet
	(((a, b'), post), pre) <-
		liftGL $ runWriterT $ unprp $ runWriterT $ unpsp $ runStateT (unBuildShaderM g) b
	buildShaderStatePut b'
	preRender pre
	postShaderProgramList post
	return a

compose1 :: BuildShader m => Ast -> m String
compose1 ast = case ast of
	Val s -> liftBuildShaderExt s
	Fn "[]" (p1:p2:[]) -> liftM2 (\a b -> a ++ "[" ++ b ++ "]")
		(compose1 p1) (compose1 p2)
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



-- Expr functions ------------------------------------------------------------------------

liftE :: String -> Expr e a1 -> Expr e a2
liftE s (Expr a) = Expr $ Fn s [a]

liftE2 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3
liftE2 s (Expr a) (Expr b) = Expr $ Fn s [a,b]

liftE3 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
liftE3 s (Expr a) (Expr b) (Expr c) = Expr $ Fn s [a,b,c]

liftE4 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4 -> Expr e a5
liftE4 s (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fn s [a,b,c,d]


instance Num a => Num (Expr e a) where
	(+) = liftE2 "+"
	(*) = liftE2 "*"
	(-) = liftE2 "-"
	abs = liftE "abs"
	signum = liftE "sign"
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

modf :: Expr e Float -> Expr e Float -> Expr e Float
modf = liftE2 "mod"

equot, erem, ediv, emod :: Expr e Int -> Expr e Int -> Expr e Int
equot = liftE2 "/"
erem = liftE2 "rem"
ediv = liftE2 "div"
emod = liftE2 "mod"


-- Attributes (VAO) ----------------------------------------------------------------------

newtype AttribM m a = AttribM { unAttrib :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, MonadIO
		, MonadGL, PostShaderProgram, PreRender, BuildShader
		)

class Monad m => Attrib m where
	offset :: m Int
	advanceBy :: Storable s => s -> m ()

instance Monad m => Attrib (AttribM m) where
	offset = AttribM get
	advanceBy a = AttribM $ modify (sizeOf a +)

-- ~ instance (Monad m) => MonadFail m where -- should get this removed
		-- ~ fail = return . error

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

instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int (Expr V Int) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

instance AttrType (Normalized Float) (Expr V Float) where
	setAttribute i a = fmap2 unNormalized $ setupAttribute1 i a

instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute s _ = liftM2 (,) (setAttribute s (err :: a)) (setAttribute s (err :: b))

instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	setAttribute s _ = liftM3 (,,)
		(setAttribute s (err :: a))
		(setAttribute s (err :: b))
		(setAttribute s (err :: c))

instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	AttrType (a,b,c,d) (x,y,z,w) where
	setAttribute s _ = liftM4 (,,,)
		(setAttribute s (err :: a))
		(setAttribute s (err :: b))
		(setAttribute s (err :: c))
		(setAttribute s (err :: d))

instance (Storable (v a), Vector v, GLtype (v a)) => AttrType (v a) (v (Expr V a)) where
	setAttribute s a = vecParts <$> setupAttribute1 s a

instance (Storable (v (v a)), Vector v, GLtype (v (v a))) =>
	AttrType (v (v a)) (v (v (Expr V a))) where
	setAttribute s a = (fmap vecParts . vecParts) <$> setupAttribute1 s a

instance (Storable a, GLtype a, KnownNat s) => AttrType (Arr s a) (Expr V (Arr s a)) where
	setAttribute s a = setupAttribute1 s a


vecParts :: forall e v a . Vector v => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill err $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall v e a . (Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = Expr $ Fn (glCName (err :: v a)) $ map ast $ toList v

expr :: Show a => a -> Expr e a
expr x = Expr $ Val $ return $ show x

arrV :: Vector v => Expr e (v a) -> Expr e Int -> Expr e a
arrV = liftE2 "[]"



setupAttribute1
	:: (GLtype a, Storable a, BuildShader m, Attrib m)
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

-- Uploadable (Uniforms) -----------------------------------------------------------------

data Var a = Var { varAst :: Ast, varMVar :: MVar a }

makeVar' :: (MonadGL m, GLtype a) => a -> m (Var a)
makeVar' a = do
	v <- makeVar
	liftIO $ fuzzySwapMVar (varMVar v) a
	return v

class Use a e where
	use :: a -> e

instance Use (Var Float) (Expr e Float) where
	use = Expr . varAst

instance Use (Var Int) (Expr e Int) where
	use = Expr . varAst

instance (Vector v, GLtype (v a), GLtype a) => Use (Var (v a)) (v (Expr e a)) where
	use v = vecParts $ Expr $ varAst v

instance (Vector v, GLtype (v (v a)), GLtype a)
	=> Use (Var (v (v a))) (v (v (Expr e a))) where
	use v = vecParts <$> vecParts (Expr $ varAst v)

instance (KnownNat s, GLtype a) => Use (Var (Arr s a)) (Expr e (Arr s a)) where
	use = Expr . varAst


makeVar :: forall a m . (MonadGL m, GLtype a) => m (Var a)
makeVar = do
	let a = (err :: a)
	m <- liftIO $ newEmptyMVar
	vname <- ((glShortName a)++) <$> generateName a
	let r = do
		addHeader "uniform" a vname
		s <- getShader
		postShaderProgram vname $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged (glUpload l)
			when (l >= 0) $ preRender $ liftIO (tryReadMVar m) >>= maybe (pure ()) (runwc wc)
		return vname
	return $ Var (Val r) m

putVar :: MonadGL m => Var a -> a -> m ()
putVar v a = liftIO . void $ fuzzySwapMVar (varMVar v) a

readVar :: MonadGL m => Var a -> m (Maybe a)
readVar = liftIO . tryReadMVar . varMVar

modifyVar :: MonadGL m => Var a -> (a -> a) -> m ()
modifyVar v f = readVar v >>= maybe (return ()) (putVar v . f)

modifyVar' :: MonadGL m => Var a -> (a -> m a) -> m ()
modifyVar' v f = readVar v >>= maybe (return ()) (\x -> f x >>= putVar v)


-- Array stub ----------------------------------------------------------------------------

data Arr (s :: Nat) a = Arr
	{ changeToken :: Int
	, unArr :: StorableArray Int a
	}

sizeArr :: forall n s a . (KnownNat s, Num n) => (Arr s a) -> n
sizeArr _ = itoi (natVal (Proxy :: Proxy s))

sizeArr' :: forall n s a p . (KnownNat s, Num n) => p (Arr s a) -> n
sizeArr' _ = itoi (natVal (Proxy :: Proxy s))

newArr :: forall m s a . (KnownNat s, Storable a, MonadIO m) => m (Arr s a)
newArr = liftIO $ Arr 0 <$> newArray_ (0, pred $ itoi (natVal (Proxy :: Proxy s)))

-- kinda cursed solution for tracking changes for Eq
modifyArr :: MonadIO m => Arr s a -> (StorableArray Int a -> m b) -> m (Arr s a)
modifyArr (Arr i sa) f = do
	f sa
	return $ Arr (succ i) sa

instance Eq (Arr s a) where
	(Arr i _) == (Arr i2 _) = i == i2

instance (Storable e, KnownNat s) => Storable (Arr s e) where
	sizeOf a = sizeArr a * sizeOf (err :: e)
	alignment _ = alignment (err :: e)
	peek p = liftIO $ do
		ar <- newArr
		modifyArr ar (\sa -> withStorableArray sa $ \p2 -> copyArray (castPtr p) p2 (sizeArr ar))
	poke p a@(Arr _ sa) = withStorableArray sa $ \ p2 -> copyArray p2 (castPtr p) (sizeArr a)
	-- i cant help but feel that this is borked


arr :: Expr e (Arr a) -> Int -> Expr e a
arr e n = liftE2 "[]" e $ expr n

-- | arr' is ignoring constant expression requirement. May not work with old GL versions.
arr' :: Expr e (Arr a) -> Expr e Int -> Expr e a
arr' = liftE2 "[]"


-- Rasterization -------------------------------------------------------------------------

class Raster a b where
	transfer :: a -> WriterT (AstM ()) AstM b

instance Raster () () where
	transfer e = return ()

instance GLtype a => Raster (Expr V a) (Expr F a) where
	transfer e = do
		let a = err :: a
		n <- generateName a
		compose n e
		addHeader "varying" a $ n
		tell $ do
			addHeader "in" a n
		return $ Expr $ Val $ return n

instance (Vector v, GLtype (v a)) => Raster (v (Expr V a)) (v (Expr F a)) where
	transfer e = do
		let va = err :: v a
		n <- generateName va
		compose n $ exprVec e
		addHeader "varying" va $ n
		tell $ do
			addHeader "in" va n
		return $ vecParts $ Expr $ Val $ return n

instance (Vector v, GLtype (v a), GLtype (v (v a))) => Raster (v (v (Expr V a))) (v (v (Expr F a))) where
	transfer e = do
		let vva = err :: v (v a)
		n <- generateName vva
		compose n $ exprVec $ fmap exprVec e
		addHeader "varying" vva $ n
		tell $ do
			addHeader "in" vva n
		return $ fmap vecParts $ vecParts $ Expr $ Val $ return n

instance (Raster a b, Raster c d) => Raster (a,c) (b,d) where
	transfer (a,b) = liftM2 (,) (transfer a) (transfer b)

instance (Raster a b, Raster c d, Raster e f) => Raster (a,c,e) (b,d,f) where
	transfer (a,b,c) = liftM3 (,,) (transfer a) (transfer b) (transfer c)


instance (Raster a b, Raster c d, Raster e f, Raster g h)
	=> Raster (a,c,e,g) (b,d,f,h) where
	transfer (a,b,c,d) = liftM4 (,,,)
		(transfer a) (transfer b) (transfer c) (transfer d)



instance Semigroup (AstM a) where
	(<>) = (>>)

instance Monoid (AstM a) where
	mempty = return $ error ""

-- Compilation ---------------------------------------------------------------------------

data RenderSet a v f = Raster v f => RenderSet
	{ shaderV :: (a -> (V4 (Expr V Float), v))
	, shaderF :: (f -> V4 (Expr F Float))
	} --, renderConfig :: RenderConfig
	-- if the resulting texture can be of different type, make it a type variable

-- perhaps its better to keep the shader in a writerT-like state like the raster function
compile :: forall a b m v f. (MonadGL m, AttrType a b, Raster v f)
	=> (b -> (V4 (Expr V Float), v))
	-> (f -> V4 (Expr F Float))
	-> m ([VArray a] -> m ())
compile sv sf = do
	sp <- glCreateProgram
	let g = do
		(i,ef,sf') <- addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes sp (err :: a)
			let (v,r) = sv e
			(ef,sf') <-runWriterT (transfer r)
			v' <- fmap Expr $ shrinkEnds $ ast $ exprVec v
			compose "gl_Position" $ v'
			return (i,ef,sf')
		addShader sp GL_FRAGMENT_SHADER $ do
			compose "gl_FragColor" $ exprVec $ sf ef
			sf'
		return i
	(vao,exec) <- liftGL $ collectPreRender $ runPostShaderProgram g
	return $ \varrs -> do
		glBindVertexArray vao
		glUseProgram sp
		liftGL $ exec
		drawArrays varrs




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
	debug $	liftIO $ putStrLn str
	liftIO $ bracket (newCAString str) free $ \cs -> do
		i <- glCreateShader t
		with cs $ \p -> glShaderSource i 1 p nullPtr
		glCompileShader i
		checkShaderError str i
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


-- Ast optimizations ---------------------------------------------------------------------
-- without context probably quick to break stuff

-- shrinkEnds currently not useful
shrinkEnds :: BuildShader m => Ast -> m Ast
shrinkEnds e@(Fn vn asts)
	| findInStringDepth 1 "vec" vn
	, Just ns <- forM asts f
	= do
		let ops@(op:_) = map tsnd ns
		ss <- liftBuildShaderExt $ sequence $ map tfst ns
		if same ops && and (zipWith (==) (map read ss) [0..])
			then fmap (Fn op) $ mapM (shrinkEnds . Fn vn) $ transpose $ map ttrd ns
			else return e
	where
		f (Fn "arr" [Val n, Fn op a]) = Just (n,op,a)
		f _ = Nothing
shrinkEnds e = return e

{-
foo = Fn "vec2"
	[ Fn "arr" [Val (return 0), Fn "+" [a1,a2]]
	, Fn "arr" [Val (return 1), Fn "+" [b1,b2]]
	]
bar = (Fn "+" [Fn "vec2" [a1,b1], Fn "vec2" [a2,b2]] ==) <$> shrinkEnds foo
-}


same :: Eq a => [a] -> Bool
same (x:y:xs) = x == y && same (y:xs)
same _ = True

findInStringDepth :: Int -> String -> String -> Bool
findInStringDepth i s = any (isPrefixOf s) . take i . tails

-- Blending ------------------------------------------------------------------------------

-- function to take a list of renders with a depth/masc/blend operator added
compileStack = undefined



-- VBO manager ---------------------------------------------------------------------------

data VBOMan = VBOMan
	{ pager :: Pager GLintptr
	, vboIndex :: GLuint
	}

initVBOMan :: GLintptr -> IO VBOMan
initVBOMan s = do
	vboMan <- withPtr_ $ glGenBuffers 1
	glBindBuffer GL_ARRAY_BUFFER vboMan
	glBufferData GL_ARRAY_BUFFER s nullPtr GL_STATIC_DRAW
	return $ VBOMan (newPager s) vboMan

getVBO :: MonadGL m => m GLuint
getVBO = liftIO . fmap vboIndex . readMVar =<< vboMan <$> glState

getPager :: MonadGL m => m (Pager GLintptr)
getPager = liftIO . fmap pager . readMVar =<< vboMan <$> glState

updatePager :: MonadGL m => (Pager GLintptr -> m (Pager GLintptr, a)) -> m a
updatePager f = do
	vmm <- vboMan <$> glState
	vm <- liftIO $ takeMVar vmm
	(p',r) <- f $ pager vm
	liftIO $ putMVar vmm $ vm { pager = p' }
	return r

putPager :: MonadGL m => Pager GLintptr -> m ()
putPager a = updatePager $ \_ -> return (a, ())

vboUpdate :: (MonadGL m, Storable a) => VArray a -> StorableArray Int a -> m ()
vboUpdate (VArray s i) a =
	liftIO $ withStorableArray a $ \p -> glBufferSubData GL_ARRAY_BUFFER i s $ castPtr p


vboAlloc :: MonadGL m => GLintptr -> GLintptr -> m GLintptr
vboAlloc a i = do
	pager <- getPager
	let maybeP = calcAlloc a pager i
	case maybeP of
		Just (pager', p) -> do
			putPager pager'
			return p
		Nothing -> do
			liftIO $ putStrLn $
				"The vboMan exceeded its allocated size. "
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
	mvm <- vboMan <$> glState
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

vArrayRange :: VArray a -> (CPtrdiff, CPtrdiff)
vArrayRange (VArray s p) = (p,s)

drawRanges :: [VArray a] -> [(CPtrdiff, CPtrdiff)]
drawRanges = condense . map vArrayRange . sortBy (comparing vArrayPos)


-- VArray interface ----------------------------------------------------------------------

data VArray a = VArray { vArraySize :: GLintptr, vArrayPos :: GLintptr } deriving (Eq,Ord)

newVArray :: (MonadGL m, Storable a, Foldable f) => f a -> m (VArray a)
newVArray xs = newVArray' =<<
	(liftIO $ newListArray (0, pred $ length xs) $ foldr (:) [] xs)


newVArray' :: (MonadGL m, Storable a) => StorableArray Int a -> m (VArray a)
newVArray' a = do
	i <- liftIO $ getNumElements a
	let s = itoi $ subSizeOf a * i
	g <- VArray s <$> vboAlloc (subSizeOf a) s
	vboUpdate g a
	return g


drawArrays :: (MonadIO m, Storable a) => [VArray a] -> m ()
drawArrays [] = return ()
drawArrays gs@(g:_) = let
		f = itoi . (`quot` (subSizeOf g))
		r = map (mapTuple f) $ drawRanges gs
	in mapM_ (uncurry (glDrawArrays GL_TRIANGLES)) r


-- | After using @removeVArray@, further calls with the given VArray are undefined.
removeVArray :: MonadGL m => VArray a -> m ()
removeVArray (VArray _ i) = updatePager $ return . (,()) . calcRemove i


-- GL extension for VAO ------------------------------------------------------------------

glGenVertexArray :: MonadIO m => m GLuint
glGenVertexArray = liftIO $ withPtr_ $ glGenVertexArraysOES 1

glBindVertexArray :: MonadIO m => GLuint -> m ()
glBindVertexArray = liftIO . glBindVertexArrayOES




-- GL type information -------------------------------------------------------------------

class Eq a => GLtype a where
	glCName :: a -> String
	glType :: a -> GLenum
	glComponents :: a -> GLint
	glComponents _ = 1
	glNormalized :: a -> GLboolean
	glNormalized _ = GL_FALSE
	glShortName :: a -> String
	glShortName a = take 1 $ glCName a
	glUpload :: MonadIO m => GLint -> a -> m ()
	-- ~ glUploads :: MonadIO m => Arr s a -> m () -- TODO
	glPrecision :: a -> String
	glPrecision _ = "highp"
	glCNameWithPrec :: a -> String
	glCNameWithPrec a = glPrecision a ++ " " ++ glCName a

instance GLtype Bool where
	glCName _ = "bool"
	glType _ = GL_BOOL
	glUpload i b = glUniform1i i $ boolToInt b

instance GLtype Int where
	glCName _ = "int"
	glType _ = GL_INT
	glUpload i = glUniform1i i . itoi

instance GLtype Float where
	glCName _ = "float"
	glType _ = GL_FLOAT
	glUpload = glUniform1f

instance GLtype (V2 Float) where
	glCName _ = "vec2"
	glType _ = GL_FLOAT
	glComponents _ = 2
	glUpload i (V2 a b) = glUniform2f i a b

instance GLtype (V3 Float) where
	glCName _ = "vec3"
	glType _ = GL_FLOAT
	glComponents _ = 3
	glUpload i (V3 a b c) = glUniform3f i a b c

instance GLtype (V4 Float) where
	glCName _ = "vec4"
	glType _ = GL_FLOAT
	glComponents _ = 4
	glUpload i (V4 a b c d) = glUniform4f i a b c d


instance GLtype (V2 Int) where
	glCName _ = "ivec2"
	glType _ = GL_INT
	glComponents _ = 2
	glUpload i (V2 a b) = glUniform2i i (itoi a) (itoi b)

instance GLtype (V3 Int) where
	glCName _ = "ivec3"
	glType _ = GL_INT
	glComponents _ = 3
	glUpload i (V3 a b c) = glUniform3i i (itoi a) (itoi b) (itoi c)

instance GLtype (V4 Int) where
	glCName _ = "ivec4"
	glType _ = GL_INT
	glComponents _ = 4
	glUpload i (V4 a b c d) = glUniform4i i (itoi a) (itoi b) (itoi c) (itoi d)


instance GLtype (V2 Bool) where
	glCName _ = "bvec2"
	glType _ = GL_BOOL
	glComponents _ = 2
	glUpload i (V2 a b) = glUniform2i i (boolToInt a) (boolToInt b)

instance GLtype (V3 Bool) where
	glCName _ = "bvec3"
	glType _ = GL_BOOL
	glComponents _ = 3
	glUpload i (V3 a b c) = glUniform3i i (boolToInt a) (boolToInt b) (boolToInt c)

instance GLtype (V4 Bool) where
	glCName _ = "bvec4"
	glType _ = GL_BOOL
	glComponents _ = 4
	glUpload i (V4 a b c d) = glUniform4i i (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)

boolToInt :: Bool -> Int32
boolToInt True = 1
boolToInt _ = 0


instance GLtype (Mat V2 V2 Float) where
	glCName _ = "mat2"
	glType _ = GL_FLOAT
	glComponents _ = 4
	glUpload i (V2 (V2 a b) (V2 c d)) = glUniform4f i a b c d

instance GLtype (Mat V3 V3 Float) where
	glCName _ = "mat3"
	glType _ = GL_FLOAT
	glComponents _ = 9
	glUpload i m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv i 1 GL_FALSE p

instance GLtype (Mat V4 V4 Float) where
	glCName _ = "mat4"
	glType _ = GL_FLOAT
	glComponents _ = 16
	glUpload i m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv i 1 GL_FALSE p

withArray' :: (MonadIO m, Storable a) => [a] -> (Ptr a -> IO b) -> m b
withArray' = liftIO .: withArray


data Normalized a = Normalized { unNormalized :: a } deriving (Eq)

instance Functor Normalized where
	fmap f (Normalized a) = Normalized $ f a

instance Storable a => Storable (Normalized a) where
	sizeOf _ = sizeOf (err :: a)
	alignment _ = alignment (err :: a)
	peek p = fmap Normalized $ peek $ castPtr p
	poke p (Normalized a) = poke (castPtr p) a

instance GLtype a => GLtype (Normalized a) where
	glNormalized _ = GL_TRUE
	glCName _ = glCName (err :: a)
	glType _ = glType (err :: a)
	glComponents _ = glComponents (err :: a)
	glUpload i _ = glUpload i (err :: a)


instance (KnownNat s, GLtype e) => GLtype (Arr s e) where
	glCName a = glCName (err :: e) ++ "[" ++ show (sizeArr a) ++ "]"
	glType _ = glType (err :: e)
	glComponents a = glComponents (err :: e) * sizeArr a
	glUpload i a = liftIO $ withStorableArray (unArr a) $ \p -> glUniform1fv i (glComponents a) $ castPtr p

