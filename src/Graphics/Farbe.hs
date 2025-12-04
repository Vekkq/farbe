{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Window
import Graphics.Farbe.Shader
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
import Graphics.Farbe.STL
import Graphics.Farbe.Utils

import Control.Monad.IO.Class


runFarbe :: MonadIO m => HandTexT (HandVBOT m) a -> m a
runFarbe = runHandVBOT (2^24) . runHandTexT

foo :: (MonadIO m) => m ()
foo = runFarbe $ do
	let (u,arr',i) = undefined
	a <- loadSTL "test/teapot.stl"
	f <- compile $ \v -> do
		let (V3 x y z) = v*0.02
		let pos = V4 x y z 1
		x' <- transfer x
		return (pos, V4 (use u `arr'` use i) x' 1 1)

	f [a]

use = undefined


-- ~ import Graphics.Farbe.Utils

{-

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.Window
import Graphics.Farbe.Utils


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Function
import Data.Foldable
import Data.Array.IO
import Data.Array.Storable
import Data.Array.Base
import Data.Array.MArray as MA
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

import System.Mem.StableName

-- ~ import Data.Typeable

-- ~ import Debug.Trace

-- GL Monad ------------------------------------------------------------------------------

data GLState = GLState
	{ glConfig :: GLConfig
	, counter :: MVar Int
	, vboMan :: MVar VBOMan
	, texUnits :: MVar (Word32, IOUArray Word32 GLuint) -- (last used, array)
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
	tex <- liftIO $ newMVar =<< initTexUnits
	runReaderT m $ GLState conf counter vbom tex

data GLConfig = GLConfig
	{ glVBOSize :: GLintptr
	, glDebug :: Bool }

glDefaultConfig :: GLConfig
glDefaultConfig = GLConfig { glVBOSize = (2^24), glDebug = True }

initTexUnits :: IO (Word32, IOUArray Word32 GLuint)
initTexUnits = do
	i <- withPtr_ $ glGetIntegerv GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	ar <- MA.newArray (1, itoi $ i `quot` 3) 0
	return $ (1, ar)



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
	-- ~ | ValSet String

type AstM = BuildShaderM (PostShaderProgramM (PreRenderM (GL IO)))

instance Semigroup (AstM a) where
	(<>) = (>>)

instance Monoid (AstM a) where
	mempty = return $ error ""

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

class ShaderType a
instance ShaderType V
instance ShaderType F

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

liftE0 :: String -> Expr e a
liftE0 s = Expr $ Fn s []

liftE1 :: String -> Expr e a1 -> Expr e a2
liftE1 s (Expr a) = Expr $ Fn s [a]

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
	abs = liftE1 "abs"
	signum = liftE1 "sign"
	fromInteger = Expr . Val . return . ($ []) . showFFloat Nothing . fromInteger

instance Fractional a => Fractional (Expr e a) where
	fromRational = Expr . Val . return . ($ []) . showFFloat Nothing . fromRat
	(/) = liftE2 "/"

napier :: Fractional a => a
napier = fromRational 2.718281828459045235360287471352

-- | Unicode alias for Napier's constant
e :: Fractional a => a
e = napier

instance Floating a => Floating (Expr e a) where
	pi = Expr $ Val $ return $ show pi
	exp = liftE1 "exp"
	log = liftE1 "log"
	sqrt = liftE1 "sqrt"
	(**) = liftE2 "^"
	sin = liftE1 "sin"
	cos = liftE1 "cos"
	tan = liftE1 "tan"
	asin = liftE1 "asin"
	acos = liftE1 "acos"
	atan = liftE1 "atan"
	-- following functions not available in glsl es 1
	sinh x = (e ** x - e ** (negate x)) / 2
	cosh x = (e ** x + e ** (negate x)) / 2
	tanh x = sinh x / cosh x
	asinh x = ln (x + sqrt (x**2 + 1))
	acosh x = ln (x + sqrt (x**2 - 1))
	atanh x = 1/2 * ln ((1+x) / (1-x))

ln :: Floating a => a -> a
ln = logBase e

modf :: Expr e Float -> Expr e Float -> Expr e Float
modf = liftE2 "mod"

equot, erem, ediv, emod :: Expr e Int32 -> Expr e Int32 -> Expr e Int32
equot = liftE2 "/"
erem = liftE2 "rem"
ediv = liftE2 "div"
emod = liftE2 "mod"

-- TODO add non-component-wise vector and matrix functions

fragCoord :: V4 (Expr F Float)
fragCoord = vecParts $ liftE0 "gl_FragCoord"

-- ~ vertexId :: Expr V Int -- not part of opengl es 2 / glsl es 1.0
-- ~ vertexId = liftE0 "gl_VertexID"


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

class Storable a => AttrType a b | a -> b, b -> a where
	setAttribute :: (BuildShader m, Attrib m) => Shader -> a -> m b

instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

-- ~ instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	-- ~ setAttribute i a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 i a

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

-- ~ instance (Storable (v a), Vector v, GLtype (v a)) => AttrType (v a) (v (Expr V a)) where
	-- ~ setAttribute s a = vecParts <$> setupAttribute1 s a

attribPartsVec :: (BuildShader m, Attrib m, GLtype (v a), Storable (v a), Vector v)
	=> Shader -> v a -> m (v (Expr V a))
attribPartsVec s a = vecParts <$> setupAttribute1 s a

instance AttrType (V2 Float) (V2 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V2 Int32) (V2 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V2 Bool) (V2 (Expr V Bool)) where setAttribute = attribPartsVec

instance AttrType (V3 Float) (V3 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V3 Int32) (V3 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V3 Bool) (V3 (Expr V Bool)) where setAttribute = attribPartsVec

instance AttrType (V4 Float) (V4 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V4 Int32) (V4 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V4 Bool) (V4 (Expr V Bool)) where setAttribute = attribPartsVec

-- ~ instance (Storable (v (v a)), Vector v, GLtype (v (v a))) =>
	-- ~ AttrType (v (v a)) (v (v (Expr V a))) where
	-- ~ setAttribute s a = (fmap vecParts . vecParts) <$> setupAttribute1 s a

attribPartsMat :: (BuildShader m, Attrib m, GLtype (v (v a)), Storable (v (v a)), Vector v)
	=> Shader -> v (v a) -> m (v (v (Expr V a)))
attribPartsMat s a = (fmap vecParts . vecParts) <$> setupAttribute1 s a

instance AttrType (V2 (V2 Float)) (V2 (V2 (Expr V Float))) where
	setAttribute = attribPartsMat

instance AttrType (V3 (V3 Float)) (V3 (V3 (Expr V Float))) where
	setAttribute = attribPartsMat

instance AttrType (V4 (V4 Float)) (V4 (V4 (Expr V Float))) where
	setAttribute = attribPartsMat



-- "disallowed by spec"
-- ~ instance (Storable a, GLtype a, KnownNat s) => AttrType (Arr s a) (Expr V (Arr s a)) where
	-- ~ setAttribute s a = setupAttribute1 s a


vecParts :: forall e v a . Vector v => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill err $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall v e a . (Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = Expr $ Fn (glCName (err :: v a)) $ map ast $ toList v

exprMat :: forall v e a . (Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr e a)) -> Expr e (v (v a))
exprMat v = Expr $ Fn (glCName (err :: v a)) $ map ast $ concatMap toList $ toList v

expr :: Show a => a -> Expr e a
expr x = Expr $ Val $ return $ show x

exprAny :: String -> Expr e a
exprAny = Expr . Val . return

arrV :: Vector v => Expr e (v a) -> Expr e Int32 -> Expr e a
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


makeVar :: forall a m . (MonadGL m, GLtype a) => a -> m (Var a)
makeVar a = do
	m <- liftIO $ newMVar a
	vname <- ((glShortName a)++) <$> generateName a
	let r = do
		addHeader "uniform" a vname
		s <- getShader
		postShaderProgram vname $ do
			l <- withString vname $ glGetUniformLocation s
			setupUpload l m
		return vname
	return $ Var (Val r) m

setupUpload' :: (Eq a, MonadGL m, PreRender m) => (a -> GL IO ()) -> MVar a -> m ()
setupUpload' f m = do
	wc <- makeRunWhenChanged f
	preRender $ (liftIO $ readMVar m) >>= runwc wc


swapVar :: MonadIO m => Var a -> a -> m a
swapVar v = liftIO . swapMVar (varMVar v)

readVar :: MonadIO m => Var a -> m a
readVar = liftIO . readMVar . varMVar



makeVarF :: MonadGL m => Float -> m (Var Float)
makeVarI :: MonadGL m => Int32 -> m (Var Int32)
makeVarB :: MonadGL m => Bool -> m (Var Bool)
makeVarV2F :: MonadGL m => V2 Float -> m (Var (V2 Float))
makeVarV2I :: MonadGL m => V2 Int32 -> m (Var (V2 Int32))
makeVarV2B :: MonadGL m => V2 Bool -> m (Var (V2 Bool))
makeVarV3F :: MonadGL m => V3 Float -> m (Var (V3 Float))
makeVarV3I :: MonadGL m => V3 Int32 -> m (Var (V3 Int32))
makeVarV3B :: MonadGL m => V3 Bool -> m (Var (V3 Bool))
makeVarV4F :: MonadGL m => V4 Float -> m (Var (V4 Float))
makeVarV4I :: MonadGL m => V4 Int32 -> m (Var (V4 Int32))
makeVarV4B :: MonadGL m => V4 Bool -> m (Var (V4 Bool))
makeVarM2 :: MonadGL m => (V2 (V2 Float)) -> m (Var (V2 (V2 Float)))
makeVarM3 :: MonadGL m => (V3 (V3 Float)) -> m (Var (V3 (V3 Float)))
makeVarM4 :: MonadGL m => (V4 (V4 Float)) -> m (Var (V4 (V4 Float)))

makeVarF   = makeVar
makeVarI   = makeVar
makeVarB   = makeVar
makeVarV2F = makeVar
makeVarV2I = makeVar
makeVarV2B = makeVar
makeVarV3F = makeVar
makeVarV3I = makeVar
makeVarV3B = makeVar
makeVarV4F = makeVar
makeVarV4I = makeVar
makeVarV4B = makeVar
makeVarM2  = makeVar
makeVarM3  = makeVar
makeVarM4  = makeVar


class Use a e r | a e -> r, r -> a e where
	use :: a -> r

instance Use (Var Float) e (Expr e Float) where use = Expr . varAst
instance Use (Var Int32) e (Expr e Int32) where use = Expr . varAst
instance Use (Var Bool) e (Expr e Bool) where use = Expr . varAst

usePartsVec :: Vector v => Var (v a) -> v (Expr e a)
usePartsVec = vecParts . Expr . varAst

instance Use (Var (V2 Float)) e (V2 (Expr e Float)) where use = usePartsVec
instance Use (Var (V2 Int32)) e (V2 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V2 Bool)) e (V2 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V3 Float)) e (V3 (Expr e Float)) where use = usePartsVec
instance Use (Var (V3 Int32)) e (V3 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V3 Bool)) e (V3 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V4 Float)) e (V4 (Expr e Float)) where use = usePartsVec
instance Use (Var (V4 Int32)) e (V4 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V4 Bool)) e (V4 (Expr e Bool)) where use = usePartsVec

usePartsMat :: Vector v => Var (v (v a)) -> v (v (Expr e a))
usePartsMat v = vecParts <$> vecParts (Expr $ varAst v)

instance Use (Var (V2 (V2 Float))) e (V2 (V2 (Expr e Float))) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) e (V3 (V3 (Expr e Float))) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) e (V4 (V4 (Expr e Float))) where use = usePartsMat

instance (KnownNat s, GLtype a) => Use (Var (Arr s a)) e (Expr e (Arr s a)) where
	use = Expr . varAst


-- Array stub ----------------------------------------------------------------------------


data Arr (s :: Nat) a = Arr
	{ changeToken :: Int
	, unArr :: StorableArray Int a
	}

sizeArr :: forall n s a . (KnownNat s, Num n) => (Arr s a) -> n
sizeArr _ = itoi (natVal (Proxy :: Proxy s))

sizeArr' :: forall n s a p . (KnownNat s, Num n) => p (Arr s a) -> n
sizeArr' _ = itoi (natVal (Proxy :: Proxy s))

newArr :: forall m s a . (KnownNat s, Storable a, MonadIO m) => [a] -> m (Arr s a)
newArr l = liftIO $ Arr 0 <$> newListArray (0, pred $ itoi (natVal (Proxy :: Proxy s))) l

emptyArr :: forall m s a . (KnownNat s, Storable a, MonadIO m) => m (Arr s a)
emptyArr = liftIO $ Arr 0 <$> newArray_ (0, pred $ itoi (natVal (Proxy :: Proxy s)))

modifyArr :: MonadIO m => Arr s a -> (StorableArray Int a -> m b) -> m (Arr s a)
modifyArr a@(Arr _ sa) f = do
	i <- hashStableName <$> liftIO (makeStableName $ f sa)
	return $ Arr i sa

readArr :: forall m s a . (KnownNat s, MonadIO m, MArray StorableArray a m)
	=> Arr s a -> m [a]
readArr (Arr _ sa) = foldrMArray' (:) [] sa

instance Eq (Arr s a) where
	(Arr i _) == (Arr i2 _) = i == i2

instance (Storable e, KnownNat s) => Storable (Arr s e) where
	sizeOf a = sizeArr a * sizeOf (err :: e)
	alignment _ = alignment (err :: e)
	peek p = liftIO $ do
		ar <- emptyArr
		modifyArr ar (\sa -> withStorableArray sa $ \p2 -> copyArray (castPtr p) p2 (sizeArr ar))
	poke p a@(Arr _ sa) = withStorableArray sa $ \p2 -> copyArray p2 (castPtr p) (sizeArr a)
	-- i cant help but feel that this is borked


arr :: Expr e (Arr s a) -> Int32 -> Expr e a
arr e n = liftE2 "[]" e $ expr n

-- | @arr'@ is ignoring constant expression requirement.
--   May not work with some implementations.
arr' :: Expr e (Arr s a) -> Expr e Int32 -> Expr e a
arr' = liftE2 "[]"




-- Compilation ---------------------------------------------------------------------------

compile :: forall a b m. (MonadGL m, AttrType a b)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- liftGL $ collectPreRender $ runPostShaderProgram $
		join $ addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes sp (err :: a)
			((v,f),fm) <- runWriterT $ f e
			compose "gl_Position" $ exprVec v
			return $ addShader sp GL_FRAGMENT_SHADER $ do
				compose "gl_FragColor" $ exprVec $ f
				fm
				return i
	return $ \varrs -> do
		glBindVertexArray vao
		glUseProgram sp
		liftGL $ exec
		drawArrays varrs


type ShaderM a = WriterT (AstM ()) AstM a


-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> ShaderM b

transfer1 :: forall a e. GLtype a => Expr e a -> ShaderM Ast
transfer1 e = do
		let a = err :: a
		n <- generateName a
		compose n e
		addHeader "varying" a $ n
		tell $ do
			addHeader "in" a n
		return $ Val $ return n

instance GLtype a => Transfer (Expr V a) (Expr F a) where
	transfer = fmap Expr . transfer1

instance GLtype (V2 a) => Transfer (V2 (Expr V a)) (V2 (Expr F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance GLtype (V3 a) => Transfer (V3 (Expr V a)) (V3 (Expr F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance GLtype (V4 a) => Transfer (V4 (Expr V a)) (V4 (Expr F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance (GLtype (V2 a), GLtype (V2 (V2 a))) => Transfer (V2 (V2 (Expr V a))) (V2 (V2 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts . Expr) . transfer1 . exprMat



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
	setupUpload :: (MonadGL m, PreRender m) => GLint -> MVar a -> m ()
	-- ~ setupUploads :: MonadIO m => Arr s a -> m () -- TODO
	glPrecision :: a -> String
	glPrecision _ = "highp"
	glCNameWithPrec :: a -> String
	glCNameWithPrec a = glPrecision a ++ " " ++ glCName a

instance GLtype Bool where
	glCName _ = "bool"
	glType _ = GL_BOOL
	setupUpload l = setupUpload' (glUniform1i l . boolToInt)

instance GLtype Int32 where
	glCName _ = "int"
	glType _ = GL_INT
	setupUpload l = setupUpload' (glUniform1i l . itoi)

instance GLtype Float where
	glCName _ = "float"
	glType _ = GL_FLOAT
	setupUpload l = setupUpload' (glUniform1f l)

instance GLtype (V2 Float) where
	glCName _ = "vec2"
	glType _ = GL_FLOAT
	glComponents _ = 2
	setupUpload l = setupUpload' (\(V2 a b) -> glUniform2f l a b)

instance GLtype (V3 Float) where
	glCName _ = "vec3"
	glType _ = GL_FLOAT
	glComponents _ = 3
	setupUpload l = setupUpload' (\(V3 a b c) -> glUniform3f l a b c)

instance GLtype (V4 Float) where
	glCName _ = "vec4"
	glType _ = GL_FLOAT
	glComponents _ = 4
	setupUpload l = setupUpload' (\(V4 a b c d) -> glUniform4f l a b c d)


instance GLtype (V2 Int32) where
	glCName _ = "ivec2"
	glType _ = GL_INT
	glComponents _ = 2
	setupUpload l = setupUpload' (\(V2 a b) -> glUniform2i l (itoi a) (itoi b))

instance GLtype (V3 Int32) where
	glCName _ = "ivec3"
	glType _ = GL_INT
	glComponents _ = 3
	setupUpload l = setupUpload' (\(V3 a b c) -> glUniform3i l (itoi a) (itoi b) (itoi c))

instance GLtype (V4 Int32) where
	glCName _ = "ivec4"
	glType _ = GL_INT
	glComponents _ = 4
	setupUpload l = setupUpload'
		(\(V4 a b c d) -> glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d))

instance GLtype (V2 Bool) where
	glCName _ = "bvec2"
	glType _ = GL_BOOL
	glComponents _ = 2
	setupUpload l = setupUpload' (\(V2 a b) -> glUniform2i l (boolToInt a) (boolToInt b))

instance GLtype (V3 Bool) where
	glCName _ = "bvec3"
	glType _ = GL_BOOL
	glComponents _ = 3
	setupUpload l = setupUpload'
		(\(V3 a b c) -> glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c))

instance GLtype (V4 Bool) where
	glCName _ = "bvec4"
	glType _ = GL_BOOL
	glComponents _ = 4
	setupUpload l = setupUpload' (\(V4 a b c d) ->
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d))

boolToInt :: Bool -> Int32
boolToInt True = 1
boolToInt _ = 0


instance GLtype (Mat V2 V2 Float) where
	glCName _ = "mat2"
	glType _ = GL_FLOAT
	glComponents _ = 4
	setupUpload l = setupUpload' (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance GLtype (Mat V3 V3 Float) where
	glCName _ = "mat3"
	glType _ = GL_FLOAT
	glComponents _ = 9
	setupUpload l = setupUpload'
		(\m -> withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p)

instance GLtype (Mat V4 V4 Float) where
	glCName _ = "mat4"
	glType _ = GL_FLOAT
	glComponents _ = 16
	setupUpload l = setupUpload'
		(\m -> withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p)

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
	-- ~ setupUpload l (Normalized e) = setupUpload l e
	setupUpload = undefined
	-- TODO

instance (KnownNat s, GLtype e) => GLtype (Arr s e) where
	glCName a = glCName (err :: e) ++ "[" ++ show (sizeArr a) ++ "]"
	glType _ = glType (err :: e)
	glComponents a = glComponents (err :: e) * sizeArr a
	setupUpload l = setupUpload'
		(\a -> liftIO $ withStorableArray (unArr a) $ \p -> glUniform1fv l (glComponents a) $ castPtr p)


class GLtype a => GLBaseType a
instance GLBaseType Int32
instance GLBaseType Float
instance GLBaseType Bool



-- Textures ------------------------------------------------------------------------------


data Texture f = Texture
	{ texId :: GLuint
	, texLastUnit :: GLenum
	, changeTokenT :: Int
	, width :: GLsizei
	, height :: GLsizei
	} deriving Eq


instance Show (Texture f) where
	show = show . texId


	-- @loadTexture2Base@ requires an image with width and height at base of 2 .
	loadTexture2Base :: (MonadIO m, TextureFormat t)
		=> t -> (GLsizei, GLsizei) -> Ptr a -> m (Texture t)
	loadTexture2Base t (w,h) p = do
		tex <- liftIO $ withPtr_ $ glGenTextures 1
		glActiveTexture $ GL_TEXTURE0
		glBindTexture GL_TEXTURE_2D tex
		glTexImage2D GL_TEXTURE_2D 0 (glTex t) w h 0 (glTex t) GL_UNSIGNED_BYTE (castPtr p)
		glGenerateMipmap GL_TEXTURE_2D
		return $ Texture tex 0 0 w h



	-- ~ loadTexture' :: forall a t m. (MonadIO m, TextureFormat t) =>
		-- ~ (GLsizei, GLsizei) -> [TConfig] -> Ptr a -> m (Texture t)
	-- ~ loadTexture' = loadTexture2Base (err :: t)

data L = L
data LA = LA
data RGB = RGB
data RGBA = RGBA

class TextureFormat a where
	glTex :: (Eq n, Num n) => a -> n

instance TextureFormat L where
	glTex _ = GL_LUMINANCE

instance TextureFormat LA where
	glTex _ = GL_LUMINANCE_ALPHA

instance TextureFormat RGB where
	glTex _ = GL_RGB

instance TextureFormat RGBA where
	glTex _ = GL_RGBA


makeVarT :: MonadGL m => Texture t -> m (Var (Texture t))
makeVarT = makeVar

instance GLtype (Texture f) where
	glCName _ = "sampler2D"
	glType _ = GL_INT
	glPrecision _ = ""
	setupUpload l m = preRender $ do
		(Texture i u c w h) <- liftIO $ readMVar m -- borked TODO
		mts <- texUnits <$> glState
		(u', ts) <- liftIO $ readMVar mts
		i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
		when (i /= i') $ do
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			liftIO $ swapMVar m $ Texture i u' c w h
			u'' <- succU ts u'
			liftIO $ writeArray ts u'' i
			liftIO $ void $ swapMVar mts (u'',ts)


succU ts x = do
	let x' = succ x
	(l,h) <- liftIO $ getBounds ts
	return $ if x' >= h then l else x'


instance Use (Var (Texture f)) e (Expr e (Texture f)) where
  use = Expr . varAst

-- add expr texture shader access functions

texture :: Expr e (Texture f) -> V2 (Expr e Float) -> V4 (Expr e Float)
texture t v = vecParts $ liftE2 "texture2D" t (exprVec v)

-- ~ texUnits :: MVar (M.Map GLenum GLuint)
	--
-- ~ modifyArr :: MonadIO m => Arr s a -> (StorableArray Int a -> m b) -> m (Arr s a)
-- ~ modifyArr a@(Arr _ sa) f = do
	-- ~ i <- hashStableName <$> liftIO (makeStableName $ f sa)
	-- ~ return $ Arr i sa

-- Extras --------------------------------------------------------------------------------


frame :: MonadGL m => m (VArray (V3 Float))
frame = newVArray $
  [ (V3 1 1 0), (V3 1 (-1) 0), (V3 (-1) (-1) 0)
  , (V3 (-1) (-1) 0), (V3 (-1) 1 0), (V3 1 1 0)
  ]

-- ~ glTexSubImage2D -- the atlas-building function

-}
