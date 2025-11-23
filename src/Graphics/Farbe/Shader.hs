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
-- ~ import Graphics.Farbe.Window
-- ~ import Graphics.Farbe.Utils
import VertexArrayCopy
import TextureCopy
-- ~ import Graphics.Farbe.Texture



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


data ShaderEnv = ShaderEnv { unShaderEnv :: CounterT (WriterT (WriterT (HandTexT IO))) }


-- Tasks ---------------------------------------------------------------------------------

newtype CounterT m a = CounterT { counter :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, Alternative, MonadTrans
		, MonadReader r, MonadWriter w, MonadError e, MonadIO
		, MonadFix, MonadPlus
		)

instance MonadState s m => MonadState s (CounterT m) where
	get = lift get
	put = lift . put


instance Monad m => Semigroup (CounterT m a) where
	(<>) = (>>)

instance Monad m => Monoid (CounterT m ()) where
	mempty = return $ error ""

class Monad m => Count m where
	count :: m Int

instance Monad m => Count (CounterT m) where
	count = CounterT $ state $ \s -> (s, succ s)


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(count,Count,)

runCounterT :: Monad m => Int -> CounterT m a -> m a
runCounterT i (CounterT st) = evalStateT st i

joinCounter :: (Count m, Monad m) => CounterT m a -> m a
joinCounter c = do
	i <- count
	runCounterT i c


generateName :: Count m => String -> m String
generateName s = count >>= return . (s++) . ("_"++) . show

{-

newtype PreRenderT m a = PreRenderT { unprp :: WriterT (IO ()) m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans
		, MonadIO, Count, PostShaderProgram, HandTex
		)

class Monad m => PreRender m where
	preRender :: IO () -> m ()

instance Monad m => PreRender (PreRenderT m) where
	preRender = PreRenderT . tell

SIMPLEFUNCTION_CLASSINSTANCES(preRender,PreRender,.)

collectPreRender :: MonadIO m => PreRenderT m a -> m (a, m ())
collectPreRender (PreRenderT m) = runWriterT m


(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)

newtype PostShaderProgramT m a = PostShaderProgramT { unpsp :: WriterT [(String, PreRenderT IO ())] m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, Alternative
		, MonadIO, Count, PreRender, HandTex
		)

class Monad m => PostShaderProgram m where
	postShaderProgramList :: [(String, PreRenderT IO ())] -> m ()

instance Monad m => PostShaderProgram (PostShaderProgramT m) where
	postShaderProgramList = PostShaderProgramT . tell

SIMPLEFUNCTION_CLASSINSTANCES(postShaderProgramList,PostShaderProgram,.)

postShaderProgram :: PostShaderProgram m => String -> m () -> m ()
postShaderProgram s a = postShaderProgramList [(s,a)]

runPostShaderProgram :: (MonadIO m, Count m) => PostShaderProgramT m a -> m (a, m ())
runPostShaderProgram p = do
	(a,w) <- runWriterT $ unpsp p
	let b = sequence_ $ map snd $ nubBy ((==) `on` fst) w
	-- ~ preRender =<< (liftGL $ snd <$> collectPreRender b)
	return (a,b)


-- Shader building monad -----------------------------------------------------------------

data BuildShaderState = BuildShaderState
	{ shaderId :: Shader
	, header :: S.Set String
	, cExpr :: [AstM ()]
	}

emptyShaderState :: Shader -> BuildShaderState
emptyShaderState i = BuildShaderState i S.empty []

newtype BuildShaderT m r = BuildShaderT { unBuildShaderT :: StateT BuildShaderState m r }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, MonadTrans
		, Count, PostShaderProgram, PreRender, HandTex
		)

runBuildShader :: Shader -> BuildShaderT m a -> m (a, BuildShaderState)
runBuildShader i b = runStateT (unBuildShaderT b) $ emptyShaderState i


class (Count m, MonadIO m, PostShaderProgram m, PreRender m) => BuildShader m where
	buildShaderState :: (BuildShaderState -> (a, BuildShaderState)) -> m a

	buildShaderStateGet :: m BuildShaderState
	buildShaderStateGet = buildShaderState $ \s -> (s,s)
	buildShaderStatePut :: BuildShaderState -> m ()
	buildShaderStatePut a = buildShaderState $ \_ -> ((),a)

instance (Count m, MonadIO m, PostShaderProgram m, PreRender m) => BuildShader (BuildShaderT m) where
	buildShaderState = BuildShaderT . state

SIMPLEFUNCTION_CLASSINSTANCES(buildShaderState,BuildShader,.)

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m ()
addHeader i a n = buildShaderState $ \s -> ((),) $
		s { header = S.insert (unwords [i, glCNameWithPrec a, n, ";"]) $ header s }

-- ~ addCExpr :: BuildShader m => String -> String -> m ()
-- ~ addCExpr n sa = buildShaderState $ \s -> ((),) $
	-- ~ s { cExpr = (n ++ " = " ++ sa) : cExpr s }

getShader :: BuildShader m => m Shader
getShader = buildShaderState $ \s -> (shaderId s, s)



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


-- Attributes (VAO) ----------------------------------------------------------------------

newtype AttribM m a = AttribM { unAttrib :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, MonadIO
		, Count, PostShaderProgram, PreRender, BuildShader
		)

class Monad m => Attrib m where
	offset :: m Int
	advanceBy :: Storable s => s -> m ()

instance Monad m => Attrib (AttribM m) where
	offset = AttribM get
	advanceBy a = AttribM $ modify (sizeOf a +)


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
	n <- name "a" a
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

name :: (Count m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

-}
