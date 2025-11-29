{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Farbe.Shader where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
-- ~ import Graphics.Farbe.Window
import Graphics.Farbe.Utils
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Texture
-- ~ import Graphics.Farbe.Counter


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



data Expr m e a = Expr { unExpr :: ExprEnv m }

data ExprEnv m = ExprEnv { fnName :: m String, rtype :: TypeS, fnAst :: [ExprEnv m] }
-- ´m´ holding BuildShaderT (ShaderEnv m)

data ExprS = ExprS String TypeS [ExprS]

runExprEnv :: Monad m => ExprEnv m -> m ExprS
runExprEnv (ExprEnv m r ps) = do
	s <- m
	ps' <- mapM runExprEnv ps
	return $ ExprS s r ps'

newtype DeferT m a = DeferT { unDefer :: WriterT [m ()] m a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadPlus, Attrib
		, MonadIO, Count, HandTex, BuildShader
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

type ShdrEnv m = BuildShaderT (ShaderEnv m)

newtype ShaderEnv m a = ShaderEnv { unShaderEnv :: CounterT (DeferT (DeferT m)) a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, Count, HandTex, BuildShader, Attrib
		)

instance MonadTrans ShaderEnv where
	lift = ShaderEnv . lift . lift . lift

instance Monad m => Defer (DeferT m) (ShaderEnv m) where
	defer :: DeferT m () -> ShaderEnv m ()
	defer = ShaderEnv . lift . defer

runShaderEnv :: (HandTex m, Monad m) => ShaderEnv m a -> m (a, m ())
runShaderEnv = runDeferT . runDeferT' . runCounterT 1 . unShaderEnv



newtype CounterT m a = CounterT { counter :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadFix, MonadPlus, BuildShader, HandTex, Defer m, Attrib
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


-- Shader building monad -----------------------------------------------------------------

type Shader = GLuint

data BuildShaderState = BuildShaderState
	{ shaderId :: Shader
	, header :: S.Set String
	, bexpr :: [(String, ExprS)]
	}

emptyShaderState :: Shader -> BuildShaderState
emptyShaderState i = BuildShaderState i S.empty []

newtype BuildShaderT m r = BuildShaderT { unBuildShaderT :: StateT BuildShaderState m r }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, MonadTrans
		, Count, HandTex
		)

runBuildShader :: Shader -> BuildShaderT m a -> m (a, BuildShaderState)
runBuildShader i b = runStateT (unBuildShaderT b) $ emptyShaderState i


class Monad m => BuildShader m where
	buildShaderState :: (BuildShaderState -> (a, BuildShaderState)) -> m a

	buildShaderStateGet :: m BuildShaderState
	buildShaderStateGet = buildShaderState $ \s -> (s,s)
	buildShaderStatePut :: BuildShaderState -> m ()
	buildShaderStatePut a = buildShaderState $ \_ -> ((),a)

instance Monad m => BuildShader (BuildShaderT m) where
	buildShaderState = BuildShaderT . state

SIMPLEFUNCTION_CLASSINSTANCES(buildShaderState,BuildShader,.)

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m ()
addHeader i a n = buildShaderState $ \s -> ((),) $
		s { header = S.insert (unwords [i, slNameWithPrec a, n, ";"]) $ header s }


addExpr :: BuildShader m => String -> Expr m e a -> m ()
addExpr n (Expr a) = do
	e <- runExprEnv a
	buildShaderState $ \s -> ((), s { bexpr = (n,e) : bexpr s })

getShader :: BuildShader m => m Shader
getShader = shaderId <$> buildShaderStateGet

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

class ShaderType a
instance ShaderType V
instance ShaderType F




compile :: forall a b m . (MonadIO m, HandTex m, AttrType m a b)
	=> (b -> DeferT (ShdrEnv m) (V4 (Expr (ShdrEnv m) V Float), V4 (Expr (ShdrEnv m) F Float)))
	-> m ([VArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- runShaderEnv $
		join $ addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes (err :: a)
			((vs,fs),fm) <- runDeferT $ f e
			addExpr "gl_Position" $ exprVec vs
			return $ addShader sp GL_FRAGMENT_SHADER $ do
				addExpr "gl_FragColor" $ exprVec $ fs
				fm
				return i
	return $ \varrs -> do
		glBindVertexArray vao
		glUseProgram sp
		exec
		drawArrays varrs


addShader :: (MonadIO m) => Shader -> GLenum -> BuildShaderT m a -> m a
addShader sp t shdr = do
	(a,st) <- runBuildShader sp shdr
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ toCExpr (bexpr st)
		++ "}"
	-- ~ debug $ liftIO $ putStrLn str
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


toCExpr :: [(String, ExprS)] -> String
toCExpr = undefined
	-- s ++ " = " ++ e
	-- ~ ++ unlines (map (("  "++) . (++";")) $ reverse $ toCExpr st)


-- Attributes (VAO) ----------------------------------------------------------------------

newtype AttribM m a = AttribM { unAttrib :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, MonadIO
		, Count, BuildShader, Defer n
		)

class Monad m => Attrib m where
	advanceBy :: Storable s => s -> m Int

instance Monad m => Attrib (AttribM m) where
	advanceBy a = AttribM $ do
		i <- get
		modify (sizeOf a +)
		return i

SIMPLEFUNCTION_CLASSINSTANCES(advanceBy,Attrib,.)

type Vao = GLuint

-- a -> BuildShaderT (ShaderEnv m) (GLuint, b)
-- | Make VAO
setAttributes :: (MonadIO m, AttrType m a b)
	=> a -> BuildShaderT (ShaderEnv m) (Vao, b)
setAttributes a = do
	i <- glGenVertexArray
	glBindVertexArray i
	e <- evalStateT (unAttrib $ setAttribute a) 0
	return (i, e)


setupAttribute1
	:: (GLtype a, Storable a, MonadIO m, Defer (BuildShaderT (ShaderEnv m)) (BuildShaderT (ShaderEnv m)))
	=> a
	-> AttribM (BuildShaderT (ShaderEnv m)) (Expr (BuildShaderT (ShaderEnv m)) V a)
setupAttribute1 a = do
	-- todo obtain shader value from buildshader monad instead
	s <- getShader
	n <- name "a" a
	o <- advanceBy a
	defer $ withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c
		glVertexAttribPointer p
			(glComponents a)
			(glType a)
			(glNormalized a)
			0
			(intPtrToPtr $ IntPtr o)
		glEnableVertexAttribArray p
	initExpr <- makeRunOnce $ do
		addHeader "attribute" a n
		return n
	return $ (liftExpr'' $ runOnce initExpr)

class (MonadIO m, Storable a, Defer (BuildShaderT (ShaderEnv m)) (BuildShaderT (ShaderEnv m))) => AttrType m a b where --  | a -> b, b -> a
	setAttribute :: a -> AttribM (BuildShaderT (ShaderEnv m)) b

instance (MonadIO m, Defer (BuildShaderT (ShaderEnv m)) (BuildShaderT (ShaderEnv m))) => AttrType m Bool (Expr (BuildShaderT (ShaderEnv m)) V Bool) where
	setAttribute = setupAttribute1
-- ~ instance AttrType Bool (Expr m V Bool) where setAttribute = setupAttribute1
-- ~ instance AttrType Int32 (Expr m V Int32) where setAttribute = setupAttribute1
-- ~ instance AttrType Float (Expr m V Float) where setAttribute = setupAttribute1

-- ~ instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	-- ~ setAttribute i a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 i a

-- ~ instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	-- ~ setAttribute _ = liftM2 (,) (setAttribute s (err :: a)) (setAttribute s (err :: b))

-- ~ instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	-- ~ setAttribute _ = liftM3 (,,)
		-- ~ (setAttribute s (err :: a))
		-- ~ (setAttribute s (err :: b))
		-- ~ (setAttribute s (err :: c))

-- ~ instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	-- ~ AttrType (a,b,c,d) (x,y,z,w) where
	-- ~ setAttribute _ = liftM4 (,,,)
		-- ~ (setAttribute s (err :: a))
		-- ~ (setAttribute s (err :: b))
		-- ~ (setAttribute s (err :: c))
		-- ~ (setAttribute s (err :: d))

-- ~ instance (Storable (v a), Vector v, GLtype (v a)) => AttrType (v a) (v (Expr m V a)) where
	-- ~ setAttribute s a = vecParts <$> setupAttribute1 s a

-- ~ attribPartsVec :: (BuildShader m, Attrib m, GLtype (v a), Storable (v a), Vector v)
	-- ~ => Shader -> v a -> m (v (Expr m V a))
-- ~ attribPartsVec s a = vecParts <$> setupAttribute1 s a

-- ~ instance AttrType (V2 Float) (V2 (Expr m V Float)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V2 Int32) (V2 (Expr m V Int32)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V2 Bool)  (V2 (Expr m V Bool)) where setAttribute = attribPartsVec

-- ~ instance AttrType (V3 Float) (V3 (Expr m V Float)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V3 Int32) (V3 (Expr m V Int32)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V3 Bool)  (V3 (Expr m V Bool)) where setAttribute = attribPartsVec

-- ~ instance AttrType (V4 Float) (V4 (Expr m V Float)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V4 Int32) (V4 (Expr m V Int32)) where setAttribute = attribPartsVec
-- ~ instance AttrType (V4 Bool)  (V4 (Expr m V Bool)) where setAttribute = attribPartsVec

-- ~ instance (Storable (v (v a)), Vector v, GLtype (v (v a))) =>
	-- ~ AttrType (v (v a)) (v (v (Expr m V a))) where
	-- ~ setAttribute s a = (fmap vecParts . vecParts) <$> setupAttribute1 s a

-- ~ attribPartsMat :: (BuildShader m, Attrib m, GLtype (v (v a)), Storable (v (v a)), Vector v)
	-- ~ => Shader -> v (v a) -> m (v (v (Expr m V a)))
-- ~ attribPartsMat s a = (fmap vecParts . vecParts) <$> setupAttribute1 s a

-- ~ instance AttrType (V2 (V2 Float)) (V2 (V2 (Expr m V Float))) where
	-- ~ setAttribute = attribPartsMat

-- ~ instance AttrType (V3 (V3 Float)) (V3 (V3 (Expr m V Float))) where
	-- ~ setAttribute = attribPartsMat

-- ~ instance AttrType (V4 (V4 Float)) (V4 (V4 (Expr m V Float))) where
	-- ~ setAttribute = attribPartsMat



-- "disallowed by spec"
-- ~ instance (Storable a, GLtype a, KnownNat s) => AttrType (Arr s a) (Expr V (Arr s a)) where
	-- ~ setAttribute s a = setupAttribute1 s a



liftExpr :: (Monad m, GLtype a) => String -> [ExprEnv m] -> Expr m e a
liftExpr s p = liftExpr' (return s) p

liftExpr' :: forall m e a . (Monad m, GLtype a) => m String -> [ExprEnv m] -> Expr m e a
liftExpr' s p = Expr $ ExprEnv s (toTypeS (err :: a)) p

liftExpr'' :: (Monad m, GLtype a) => m String -> Expr m e a
liftExpr'' s = liftExpr' s []

-- overload it for multiple parameters

liftE0 :: forall m e a . (Monad m, GLtype a) => String -> Expr m e a
liftE0 s = liftExpr s  []

liftE1 :: forall m e a a1 a2 . (Monad m, GLtype a2) => String -> Expr m e a1 -> Expr m e a2
liftE1 s (Expr a) = liftExpr s [a]

liftE2 :: forall m e a a1 a2 a3 . (Monad m, GLtype a3) => String -> Expr m e a1 -> Expr m e a2 -> Expr m e a3
liftE2 s (Expr a) (Expr b) = liftExpr s [a,b]

-- ~ liftE3 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
-- ~ liftE3 s (Expr a) (Expr b) (Expr c) = Expr $ Fn s [a,b,c]

-- ~ liftE4 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4 -> Expr e a5
-- ~ liftE4 s (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fn s [a,b,c,d]


vecParts :: forall m e v a . (Monad m, GLtype a, Vector v) => Expr m e (v a) -> v (Expr m e a)
vecParts e = fromListFill err $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall m v e a . (Monad m, GLtype a, Vector v, GLtype (v a)) => v (Expr m e a) -> Expr m e (v a)
exprVec v = liftExpr (slName (err :: v a)) $ map unExpr $ toList v

exprMat :: forall m v e a . (Monad m, GLtype a, Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr m e a)) -> Expr m e (v (v a))
exprMat v = liftExpr (slName (err :: v a)) $ map unExpr $ concatMap toList $ toList v

expr :: (Monad m, Show b, GLtype a) => b -> Expr m e a
expr x = liftExpr (show x) []


arrV :: (Monad m, GLtype a, Vector v) => Expr m e (v a) -> Expr m e Int32 -> Expr m e a
arrV = liftE2 "[]"


name :: (Count m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f



-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> DeferT m b

transfer1 :: forall m a e. (GLtype a, BuildShader m, Count m) => Expr m V a -> DeferT m (Expr m F a)
transfer1 e = do
		let a = err :: a
		n <- name "t" a
		-- ~ e' <- runShaderEnv e
		lift $ addExpr n e
		addHeader "varying" a $ n
		defer $ do
			addHeader "in" a n
		return $ liftExpr'' $ return n

-- ~ instance GLtype a => Transfer (Expr m V a) (Expr m F a) where
	-- ~ transfer = fmap Expr . transfer1

-- ~ instance GLtype (V2 a) => Transfer (V2 (Expr m V a)) (V2 (Expr m F a)) where
	-- ~ transfer = fmap (vecParts . Expr) . transfer1 . exprVec

-- ~ instance GLtype (V3 a) => Transfer (V3 (Expr m V a)) (V3 (Expr m F a)) where
	-- ~ transfer = fmap (vecParts . Expr) . transfer1 . exprVec

-- ~ instance GLtype (V4 a) => Transfer (V4 (Expr m V a)) (V4 (Expr m F a)) where
	-- ~ transfer = fmap (vecParts . Expr) . transfer1 . exprVec

-- ~ instance (GLtype (V2 a), GLtype (V2 (V2 a))) => Transfer (V2 (V2 (Expr m V a))) (V2 (V2 (Expr m F a))) where
	-- ~ transfer = fmap (fmap vecParts . vecParts . Expr) . transfer1 . exprMat




{-

-- Expr ----------------------------------------------------------------------------------

data Expr e r = Expr { rtype :: GLenum, astM :: AstM r }

type AstM = BuildShaderT (HandTexT (PostShaderProgramT (PreRenderT (CounterT IO))))

compile :: forall a b m. (MonadIO m, HandTex m, AttrType a b)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- liftIO $ runCounterT 1 $ collectPreRender $ runPostShaderProgram $ joinHandTex $
		join $ addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes sp (err :: a)
			((vs,fs),fm) <- runWriterT $ f e
			compose "gl_Position" $ exprVec vs
			return $ addShader sp GL_FRAGMENT_SHADER $ do
				compose "gl_FragColor" $ exprVec $ fs
				fm
				return i
	return $ \varrs -> do
		glBindVertexArray $ fst vao
		glUseProgram sp
		liftIO $ exec
		drawArrays varrs


type ShaderM a = WriterT (AstM ()) (AstM) a


-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> ShaderM b

transfer1 :: forall a e. GLtype a => Expr e a -> ShaderM a
transfer1 e = do
		let a = err :: a
		n <- name "t" a
		compose n e
		addHeader "varying" a $ n
		tell $ do
			addHeader "in" a n
		return $ Val $ return n

instance GLtype a => Transfer (Expr m V a) (Expr m F a) where
	transfer = fmap Expr . transfer1

instance GLtype (V2 a) => Transfer (V2 (Expr m V a)) (V2 (Expr m F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance GLtype (V3 a) => Transfer (V3 (Expr m V a)) (V3 (Expr m F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance GLtype (V4 a) => Transfer (V4 (Expr m V a)) (V4 (Expr m F a)) where
	transfer = fmap (vecParts . Expr) . transfer1 . exprVec

instance (GLtype (V2 a), GLtype (V2 (V2 a))) => Transfer (V2 (V2 (Expr m V a))) (V2 (V2 (Expr m F a))) where
	transfer = fmap (fmap vecParts . vecParts . Expr) . transfer1 . exprMat



-- Shader compilation --------------------------------------------------------------------

type Shader = GLuint


addShader :: (MonadIO m) => Shader -> GLenum -> BuildShaderT m a -> m a
addShader sp t shdr = do
	(a,st) <- runBuildShader sp shdr
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ unlines (map (("  "++) . (++";")) $ reverse $ cExpr st)
		++ "}"
	-- ~ debug $ liftIO $ putStrLn str
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


class Eq a => GLtype a where
	glCName :: a -> String
	glType :: a -> GLenum
	glComponents :: a -> GLint
	glComponents _ = 1
	glNormalized :: a -> GLboolean
	glNormalized _ = GL_FALSE
	glShortName :: a -> String
	glShortName a = take 1 $ glCName a
	setupUpload :: (PreRender m, HandTex m, MonadIO m) => GLint -> MVar a -> m ()
	glPrecision :: a -> String
	glPrecision _ = "highp"
	glCNameWithPrec :: a -> String
	glCNameWithPrec a = glPrecision a ++ " " ++ glCName a


------------------------------------------------------------------------------------------

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

class ShaderType a
instance ShaderType V
instance ShaderType F




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





-}
