{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Shader where

import Graphics.Farbe.Vec
import Graphics.Farbe.Tuple
import Graphics.Farbe.GL
import Graphics.Farbe.Utils
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Array
import Graphics.Farbe.Texture
import Graphics.Farbe.Window
import Graphics.Farbe.Utility
import Graphics.Farbe.Delay


import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C
import Data.Hashable
import System.Mem.StableName
import qualified Data.Sequence as Se
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

#define bottom undefined

-- | The basic order is
--   Expr to BuildShaderState to String

-- | User-facing type expression.
data Expr e a = Expr { unExpr :: ExprI } deriving Functor

-- | Internal expression description.
data ExprI = ExprI { fnName :: Shdr String, rtype :: TypeS, fnAst :: [ExprI] }

data ExprS = ExprS String TypeS [ExprS] deriving Show

-- | A Shader-building environment.
type Shdr = BuildShaderT (ShaderEnv)


runExprI :: ExprI -> Shdr ExprS
runExprI (ExprI m r ps) = do
	s <- m
	ps' <- mapM runExprI ps
	return $ ExprS s r ps'


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\


-- ~ newtype World a = World { runWorld :: DelayedT' (HandTexT IO) a }
	-- ~ deriving
		-- ~ ( Functor, Applicative, Monad, Alternative
		-- ~ , MonadPlus, MonadIO, HandTex, Delay (HandTexT IO)
		-- ~ )

type World = DelayedT' (ShaderCacheT' (HandTexT IO))
type SmallWorld = (ShaderCacheT' (HandTexT IO))

liftWorld :: (Delay SmallWorld m, ShaderCache (HandTexT IO) m, HandTex m, MonadIO m) => World a -> m a
liftWorld n = do
	t <- getTex
	sc <- shaderCacheStateGet
	(((r,seq),sc'),t') <- liftIO $ runHandTexT' t $ runShaderCache' sc $ runDelayedT n
	mapM_ delay $ toList seq
	setTex t'
	return r



type ShaderEnv = CounterT (DeferT' (DeferT' World))
-- ~ instance MonadTrans ShaderEnvT where
	-- ~ lift = ShaderEnvT . lift . lift . lift . lift . lift

runShaderEnv
	:: (HandTex m, HandTex n, MonadIO m, MonadIO n, Delay SmallWorld m, Delay SmallWorld n, ShaderCache (HandTexT IO) m, ShaderCache (HandTexT IO) n)
	=> ShaderEnv a -> m (a, n ())
runShaderEnv m = do
	(r,rm) <- liftWorld $ runDeferT $ runDeferT' $ runCounterT 1 m

	return (r, liftWorld $ sequence_ rm)


-- Shader delay monad --------------------------------------------------------------------

type DSeq n = Se.Seq (n ())

newtype DelayedT n m a = DelayedT { unDelayedT :: StateT (DSeq n) m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, Alternative
		, MonadPlus, MonadIO, Count, HandTex, HandVBO, Defer d, ShaderCache f
		, MonadReader r, MonadWriter w
		, MonadError e, MonadWindow
		)

instance MonadState s m => MonadState s (DelayedT n m) where
	get = lift get
	put = lift . put

-- ~ instance (Defer n m, Monad m) => Defer n (DelayedT r m) where
	-- ~ defer = lift . defer

type DelayedT' m = DelayedT m m

runDelayedT :: Monad m => DelayedT n m a -> m (a, DSeq n)
runDelayedT (DelayedT m) = do
	(a,w) <- runStateT m Se.empty
	return (a, w)


class Monad m => DelayedState n m where
	delayedState :: (DSeq n -> (a, DSeq n)) -> m a

	delayedStateGet :: m (DSeq n)
	delayedStateGet = delayedState $ \s -> (s,s)
	delayedStatePut :: DSeq n -> m ()
	delayedStatePut a = delayedState $ \_ -> ((),a)


instance Monad m => DelayedState n (DelayedT n m) where
	delayedState = DelayedT . state




class Delay n m | m -> n where
	delay :: n a -> m ()

instance (MonadIO n, MonadIO m) => Delay n (DelayedT n m) where
	-- ~ delay :: n a -> DelayedT n m (MVar a)
	delay n = DelayedT $ modify (|>void n)

delayR n = do
		mvar <- liftIO newEmptyMVar
		delay $ n >>= liftIO . putMVar mvar
		return mvar

-- ~ liftDelayed :: (Monad m, Delay n m) => DelayedT n m a -> m a
-- ~ liftDelayed (DelayedT (DeferT ms)) = do
	-- ~ (a,seq) <- runStateT ms mempty
	-- ~ mapM_ delay $ toList seq
	-- ~ return a

-- ~ liftDelayed' :: (Monad m, Delay n m) => DelayedT n o a -> m (o a)
-- ~ liftDelayed' (DelayedT d) = do
	-- ~ (a,seq) <- runDeferT d
	-- ~ undefined -- somethin somethin


#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(delay,Delay n,.)


-- ~ type Time = Double

-- ~ type Hash = Int

-- ~ type ShaderFn b = (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))

-- ~ data SKey = SKey Hash String deriving (Eq, Ord)

-- ~ newtype ShaderCacheT m a = ShaderCacheT
	-- ~ { runShaderCacheT :: StateT (Map SKey (Maybe Int)) m a }



-- ~ class ShaderCache m where
	-- ~ shader :: (MonadIO m, HandTex m, AttrType a b)
		-- ~ => (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
		-- ~ -> m (MVar ([VArray a] -> m ()))

-- ~ instance ShaderCache (ShaderCacheT m) where
	-- ~ shader f = do
		-- ~ g <- compile f
		-- ~ m <- newMVar g
		-- ~ return m


-- Shader Cache --------------------------------------------------------------------------

instance Eq ExprI where
	(ExprI _ r ps) == (ExprI _ r2 ps2) = r == r2 && ps == ps2

instance Hashable ExprI where
	hashWithSalt salt (ExprI _ r ps) = salt `hashWithSalt` r `hashWithSalt` ps

type Hash = Int

data CacheState w = CacheState
	{ cacheMap :: M.Map Hash (MVar (w ())) -- holds items based on StableName
	, backupMap :: M.Map Hash (MVar (w ())) -- holds items based on partial hashes
	}

emptyCacheState = CacheState M.empty M.empty

addToCache :: (ShaderCache w m, MonadIO m) => ExprI -> MVar (w ()) -> m ()
addToCache e v = do
	h <- snHash e
	shaderCacheState (\(CacheState m1 m2)
		-> ((),CacheState (M.insert h v m1) (M.insert (hash e) v m2)))

snHash :: MonadIO m => a -> m Int
snHash a = liftIO $ hashStableName <$> makeStableName a

lookupCache :: (ShaderCache w m, MonadIO m) => ExprI -> m (Maybe (MVar (w ())))
lookupCache e = do
	(CacheState m1 m2) <- shaderCacheStateGet
	h <- snHash e
	let mw = M.lookup h m1
	let mw2 = M.lookup (hash e) m2
	return $ listToMaybe $ catMaybes [mw, mw2]

newtype ShaderCacheT w m r = ShaderCacheT { unshaderCacheT :: StateT (CacheState w) m r }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, MonadTrans
		, Count, HandTex, HandVBO
		, MonadReader q, MonadWriter v
		, MonadError e, MonadWindow
		)

instance MonadState s m => MonadState s (ShaderCacheT w m) where
	get = lift get
	put = lift . put

type ShaderCacheT' m = ShaderCacheT m m

runShaderCache :: ShaderCacheT w m a -> m (a, CacheState w)
runShaderCache b = runStateT (unshaderCacheT b) emptyCacheState

runShaderCache' :: CacheState w -> ShaderCacheT w m a -> m (a, CacheState w)
runShaderCache' s b = runStateT (unshaderCacheT b) s


class Monad m => ShaderCache w m where
	shaderCacheState :: (CacheState w -> (a, CacheState w)) -> m a

	shaderCacheStateGet :: m (CacheState w)
	shaderCacheStateGet = shaderCacheState $ \s -> (s,s)
	shaderCacheStatePut :: CacheState w -> m ()
	shaderCacheStatePut a = shaderCacheState $ \_ -> ((),a)

instance Monad m => ShaderCache w (ShaderCacheT w m) where
	shaderCacheState = ShaderCacheT . state

SIMPLEFUNCTION_CLASSINSTANCES(shaderCacheState,ShaderCache n,.)


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
		, Count, HandTex, Defer n
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

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m Bool
addHeader i a n = do
	let str = unwords [i, slNameWithPrec a, n, ";"]
	s <- buildShaderStateGet
	let b = not $ S.member str (header s)
	when b $ buildShaderStatePut $ s { header = S.insert str $ header s }
	return b

addExpr :: String -> Expr e a -> Shdr ()
addExpr n (Expr a) = do
	e <- runExprI a
	buildShaderState $ \s -> ((), s { bexpr = (n,e) : bexpr s })

getShader :: BuildShader m => m Shader
getShader = shaderId <$> buildShaderStateGet

data V -- | Vertex shader signifier.
data F -- | Fragment/pixel shader signifier.

class ShaderType a
instance ShaderType V
instance ShaderType F

class (MonadIO m, Defer (DeferT' (World) ()) m) => PostShader m
instance (MonadIO m, Defer (DeferT' (World) ()) m) => PostShader m

type ShaderM = DeferT' Shdr

compile :: (MonadIO m, MonadIO n, HandTex n, HandTex m, AttrType a b, Delay SmallWorld m, Delay SmallWorld n, ShaderCache (HandTexT IO) m, ShaderCache (HandTexT IO) n)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> n ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- runShaderEnv $
		join $ addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes (bottom :: a)
			((vs,fs),fm) <- runDeferT $ f e
			addExpr "gl_Position" $ exprVec vs
			return $ addShader sp GL_FRAGMENT_SHADER $ do
				addExpr "gl_FragColor" $ exprVec $ fs
				-- splice fs here to add further outputs
				sequence_ fm
				return i
	return $ \varrs -> do
		glUseProgram sp       -- lines to back up
		glBindVertexArray vao --
		exec                  --
		drawArrays varrs


foo :: (MonadIO m, MonadIO n, HandTex n, HandTex m, AttrType a b)
	=> (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-> m (Maybe ([VArray a] -> n ()))
foo = undefined

addShader :: (MonadIO m) => Shader -> GLenum -> BuildShaderT m a -> m a
addShader sp t shdr = do
	(a,st) <- runBuildShader sp shdr
	let str
		=  "#version 100\n"
		++ unlines (toList $ header st)
		++ "\n\nvoid main(){\n"
		++ toCStatements (bexpr st)
		++ "}"
	-- ~ liftIO $ putStrLn str
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
			"" -> do
				-- ~ putStrLn str
				return ()
			e -> do
				putStrLn str
				putStrLn e


toCStatements :: [(String, ExprS)] -> String
toCStatements xs = unlines $ reverse $ for xs $ \(s,e) -> s ++ " = " ++ toCExpr e ++";"


toCExpr :: ExprS -> String
toCExpr e = case e of
	ExprS s _ [] -> s
	ExprS "[]" _ (p1:p2:[]) -> toCExpr p1 ++ "[" ++ toCExpr p2 ++ "]"
	ExprS s _ (p1:p2:[]) | isOp s -> par $ toCExpr p1 ++ s ++ toCExpr p2
	ExprS "if" _ (p1:p2:p3:[]) -> par $ toCExpr p1 ++ "?" ++ toCExpr p2 ++ ":" ++ toCExpr p3
	ExprS s _ as -> (s++) $ par $ intercalate ", " $ map toCExpr as
	where
		isOp :: String -> Bool
		isOp (x:_) = not $ isAlpha x
		isOp [] = False

		par :: String -> String
		par s = "(" ++ s ++ ")"

-- Attributes (VAO) ----------------------------------------------------------------------

newtype AttribM m a = AttribM { unAttrib :: StateT Int m a }
	deriving
		( Functor, Applicative, Monad, MonadTrans, MonadIO
		, Count, BuildShader, Defer n
		, MonadReader r
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

-- | Make VAO
setAttributes :: (AttrType a b, BuildShader m, Count m, PostShader m) => a -> m (Vao, b)
setAttributes a = do
	i <- glGenVertexArray
	glBindVertexArray i
	e <- runReaderT (evalStateT (unAttrib $ setAttribute a) 0) (sizeOf a)
	return (i, e)


setupAttribute1
	:: (GLtype a, Storable a, MonadReader Int m, Attrib m, BuildShader m, Count m, PostShader m)
	=> a
	-> m (Expr V a)
setupAttribute1 a = do
	s <- getShader
	n <- name "a" a
	entireSize <- ask
	o <- advanceBy a
	defer $ withString n $ \c -> do
		p <- fromIntegral <$> glGetAttribLocation s c
		when (p < 2^8) $ do
			glVertexAttribPointer p
				(glComponents a)
				(glType a)
				(glNormalized a)
				(itoi $ entireSize)
				(intPtrToPtr $ IntPtr o)
			glEnableVertexAttribArray p
		-- ~ liftIO $ putStrLn $ "sl pos: " ++ show p ++ "\t arr pos: " ++ show o ++ "\t stride: " ++ (show $ itoi $ entireSize - sizeOf a) ++ "\t components: " ++ (show $ glComponents a)
	return $ liftExprShdr' $ do
		addHeader "attribute" a n
		return n

class Storable a => AttrType a b | a -> b, b -> a where
	setAttribute
		:: (MonadReader Int m, Attrib m, BuildShader m, Count m, PostShader m)
		=> a -> m b


instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	setAttribute a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 a
		where
		fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
		fmap2 f = fmap (fmap f)


instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute _ = liftM2 (,) (setAttribute (bottom :: a)) (setAttribute (bottom :: b))

instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	setAttribute _ = liftM3 (,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))

instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	AttrType (a,b,c,d) (x,y,z,w) where
	setAttribute _ = liftM4 (,,,)
		(setAttribute (bottom :: a))
		(setAttribute (bottom :: b))
		(setAttribute (bottom :: c))
		(setAttribute (bottom :: d))

attribPartsVec
	:: ( GLtype a, GLtype (v a), Storable a, Storable (v a), Vector v
		 , MonadReader Int m, Attrib m, BuildShader m, Count m, PostShader m)
	=> v a -> m (v (Expr V a))
attribPartsVec a = vecParts <$> setupAttribute1 a

instance AttrType (V2 Float) (V2 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V2 Int32) (V2 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V2 Bool)  (V2 (Expr V Bool)) where setAttribute = attribPartsVec


instance AttrType (V3 Float) (V3 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V3 Int32) (V3 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V3 Bool)  (V3 (Expr V Bool)) where setAttribute = attribPartsVec

instance AttrType (V4 Float) (V4 (Expr V Float)) where setAttribute = attribPartsVec
instance AttrType (V4 Int32) (V4 (Expr V Int32)) where setAttribute = attribPartsVec
instance AttrType (V4 Bool)  (V4 (Expr V Bool)) where setAttribute = attribPartsVec


attribPartsMat
	:: ( GLtype (v (v a)), GLtype (v a), GLtype a, Storable (v (v a)), Vector v
		 , MonadReader Int m, Attrib m, BuildShader m, Count m, PostShader m)
	=> v (v a) -> m (v (v (Expr V a)))
attribPartsMat a = (fmap vecParts . vecParts) <$> setupAttribute1 a

instance AttrType (V2 (V2 Float)) (V2 (V2 (Expr V Float))) where
	setAttribute = attribPartsMat

instance AttrType (V3 (V3 Float)) (V3 (V3 (Expr V Float))) where
	setAttribute = attribPartsMat

instance AttrType (V4 (V4 Float)) (V4 (V4 (Expr V Float))) where
	setAttribute = attribPartsMat



-- "disallowed by spec"
-- ~ instance (Storable a, GLtype a, KnownNat s) => AttrType (Arr s a) (Expr V (Arr s a)) where
	-- ~ setAttribute s a = setupAttribute1 s a


------------------------------------------------------------------------------------------

expr :: (Show b, GLtype a) => b -> Expr e a
expr x = liftExpr (show x) []


liftExpr :: (GLtype a) => String -> [ExprI] -> Expr e a
liftExpr s p = liftExprShdr (return s) p

liftExpr' :: (GLtype a) => String -> Expr e a
liftExpr' s = liftExpr s []

liftExprShdr :: forall e a . (GLtype a) => Shdr String -> [ExprI] -> Expr e a
liftExprShdr s p = Expr $ ExprI s (toTypeS (bottom :: a)) p

liftExprShdr' :: (GLtype a) => Shdr String -> Expr e a
liftExprShdr' s = liftExprShdr s []


-- overload it for multiple parameters

liftE0 ::(GLtype a) => String -> Expr e a
liftE0 s = liftExpr s []

liftE1 :: (GLtype a2) => String -> Expr e a1 -> Expr e a2
liftE1 s (Expr a) = liftExpr s [a]

liftE2 :: (GLtype a3) => String -> Expr e a1 -> Expr e a2 -> Expr e a3
liftE2 s (Expr a) (Expr b) = liftExpr s [a,b]

liftE3 :: (GLtype a4) => String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4
liftE3 s (Expr a) (Expr b) (Expr c) = liftExpr s [a,b,c]

-- ~ class LiftExpr a r where
	-- ~ liftE :: a -> r

-- ~ instance LiftExpr (a -> r) r where
	-- ~ liftE = (\a -> (a:))



-- ~ liftE4 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4 -> Expr e a5
-- ~ liftE4 s (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fn s [a,b,c,d]


vecParts :: (GLtype a, Vector v) => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill bottom $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall e a v . (GLtype a, Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = liftExpr (slName (bottom :: v a)) $ map unExpr $ toList v

exprMat :: forall a e v .(GLtype a, Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr e a)) -> Expr e (v (v a))
exprMat v = liftExpr (slName (bottom :: v a)) $ map unExpr $ concatMap toList $ toList v

arrV :: (GLtype a, Vector v) => Expr e (v a) -> Expr e Int32 -> Expr e a
arrV = liftE2 "[]"


name :: (Count m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

nameAttrib :: (Count m, GLtype a) => String -> a -> m String
nameAttrib s a = (++ glShortName a) <$> generateName s

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f



-- Shader value transfer -----------------------------------------------------------------

-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> ShaderM b

instance (GLtype a) => Transfer (Expr V a) (Expr F a) where
	transfer = transfer1

instance (GLtype a, GLtype (V2 a)) => Transfer (V2 (Expr V a)) (V2 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V3 a)) => Transfer (V3 (Expr V a)) (V3 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V4 a)) => Transfer (V4 (Expr V a)) (V4 (Expr F a)) where
	transfer = fmap (vecParts) . transfer1 . exprVec

instance (GLtype a, GLtype (V2 a), GLtype (V2 (V2 a)))
	=> Transfer (V2 (V2 (Expr V a))) (V2 (V2 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat

instance (GLtype a, GLtype (V3 a), GLtype (V3 (V3 a))) =>
	Transfer (V3 (V3 (Expr V a))) (V3 (V3 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat

instance (GLtype a, GLtype (V4 a), GLtype (V4 (V4 a))) =>
	Transfer (V4 (V4 (Expr V a))) (V4 (V4 (Expr F a))) where
	transfer = fmap (fmap vecParts . vecParts) . transfer1 . exprMat


deriving instance (Monad m, BuildShader m) => BuildShader (DeferT' m)

transfer1 :: forall a . GLtype a => Expr V a -> DeferT' Shdr (Expr F a)
transfer1 e = do
	let a = bottom :: a
	n <- name "t" a
	lift $ addExpr n e
	addHeader "varying" a $ n
	defer $ void $ addHeader "in" a n
	return $ liftExprShdr' $ return n



-- Uniform variables ---------------------------------------------------------------------

data Var a = Var { varExpr :: ExprI, varMVar :: MVar a }

swapVar :: MonadIO m => Var a -> a -> m a
swapVar v = liftIO . swapMVar (varMVar v)

readVar :: MonadIO m => Var a -> m a
readVar = liftIO . readMVar . varMVar


makeVar :: forall a m . (Count m, MonadIO m, GLtype a, Upload a) => a -> m (Var a)
makeVar a = do
	m <- liftIO $ newMVar a
	vname <- (name "u" a)
	let r = do
		b <- addHeader "uniform" a vname
		s <- getShader
		when b $ defer $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged $ upload l
			-- RunWhenChanged will bork for textures, since they need to be always checked for assigned tex unit
			defer $ (liftIO $ readMVar m) >>= runwc wc
		return vname
	return $ Var (ExprI r (toTypeS (bottom :: a)) []) m


class (GLtype a, Eq a) => Upload a where
	upload :: (MonadIO m, HandTex m) => GLint -> a -> m ()
	-- TODO: makeUploadFn :: GLint -> a -> m (a -> m ())
	-- move RunWhenChanged into instances

instance Upload Bool where upload l = glUniform1i l . boolToInt
instance Upload Int32 where upload l = glUniform1i l . itoi
instance Upload Float where	upload l = glUniform1f l
instance Upload (V2 Float) where upload l (V2 a b) = glUniform2f l a b
instance Upload (V3 Float) where upload l (V3 a b c) = glUniform3f l a b c
instance Upload (V4 Float) where upload l (V4 a b c d) = glUniform4f l a b c d

instance Upload (V2 Int32) where
	upload l (V2 a b) = glUniform2i l (itoi a) (itoi b)

instance Upload (V3 Int32) where
	upload l (V3 a b c) = glUniform3i l (itoi a) (itoi b) (itoi c)

instance Upload (V4 Int32) where
	upload l (V4 a b c d) = glUniform4i l (itoi a) (itoi b) (itoi c) (itoi d)

instance Upload (V2 Bool) where
	upload l (V2 a b) = glUniform2i l (boolToInt a) (boolToInt b)

instance Upload (V3 Bool) where
	upload l (V3 a b c) = glUniform3i l (boolToInt a) (boolToInt b) (boolToInt c)

instance Upload (V4 Bool) where
	upload l (V4 a b c d) =
		glUniform4i l (boolToInt a) (boolToInt b) (boolToInt c) (boolToInt d)


instance Upload (Mat V2 V2 Float) where
	upload l = (\(V2 (V2 a b) (V2 c d)) -> glUniform4f l a b c d)

instance Upload (Mat V3 V3 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix3fv l 1 GL_FALSE p

instance Upload (Mat V4 V4 Float) where
	upload l m = withArray' (toList2 m) $ \p -> glUniformMatrix4fv l 1 GL_FALSE p

instance Upload (Texture f) where
	upload l (Texture i mu _ _ _) = do
		TexState u' ts <- getTex
		u <- liftIO $ readMVar mu
		i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
		if (i /= i') then do
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			liftIO $ swapMVar mu u'
			liftIO $ writeArray ts u' i
			u'' <- succU ts u'
			setTex $ TexState u'' ts
		else glUniform1i l $ itoi u
		where
		succU ts x = do
			let x' = succ x
			(a,b) <- liftIO $ getBounds ts
			return $ if x' >= b then a else x'

{-
instance Upload (Texture f) where
	upload l (Texture i mu _ _ _) = do
		TexState u' ts <- getTex
		u <- liftIO $ readMVar mu
		i' <- if (u == 0) then return 0 else liftIO $ readArray ts u
		if (i /= i') then do
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			liftIO $ swapMVar mu u'
			liftIO $ writeArray ts u' i
			u'' <- succU ts u'
			setTex $ TexState u'' ts
		else glUniform1i l $ itoi u
		where
		succU ts x = do
			let x' = succ x
			(a,b) <- liftIO $ getBounds ts
			return $ if x' >= b then a else x'
-}

-- makeVars ------------------------------------------------------------------------------

makeVarF :: (Count m, MonadIO m) => Float -> m (Var Float)
makeVarI :: (Count m, MonadIO m) => Int32 -> m (Var Int32)
makeVarB :: (Count m, MonadIO m) => Bool -> m (Var Bool)
makeVarV2F :: (Count m, MonadIO m) => V2 Float -> m (Var (V2 Float))
makeVarV2I :: (Count m, MonadIO m) => V2 Int32 -> m (Var (V2 Int32))
makeVarV2B :: (Count m, MonadIO m) => V2 Bool -> m (Var (V2 Bool))
makeVarV3F :: (Count m, MonadIO m) => V3 Float -> m (Var (V3 Float))
makeVarV3I :: (Count m, MonadIO m) => V3 Int32 -> m (Var (V3 Int32))
makeVarV3B :: (Count m, MonadIO m) => V3 Bool -> m (Var (V3 Bool))
makeVarV4F :: (Count m, MonadIO m) => V4 Float -> m (Var (V4 Float))
makeVarV4I :: (Count m, MonadIO m) => V4 Int32 -> m (Var (V4 Int32))
makeVarV4B :: (Count m, MonadIO m) => V4 Bool -> m (Var (V4 Bool))
makeVarM2 :: (Count m, MonadIO m) => (V2 (V2 Float)) -> m (Var (V2 (V2 Float)))
makeVarM3 :: (Count m, MonadIO m) => (V3 (V3 Float)) -> m (Var (V3 (V3 Float)))
makeVarM4 :: (Count m, MonadIO m) => (V4 (V4 Float)) -> m (Var (V4 (V4 Float)))
makeVarT :: (Count m, MonadIO m) => Texture t -> m (Var (Texture t))

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
makeVarT   = makeVar

-- add expr texture shader access functions

-- Access uniform variables --------------------------------------------------------------

class Use a e r | a e -> r, r -> a e where
	use :: a -> r

instance Use (Var Float) e (Expr e Float) where use = Expr . varExpr
instance Use (Var Int32) e (Expr e Int32) where use = Expr . varExpr
instance Use (Var Bool) e (Expr e Bool) where use = Expr . varExpr

usePartsVec :: (Vector v, GLtype a) => Var (v a) -> v (Expr e a)
usePartsVec = vecParts . Expr . varExpr

instance Use (Var (V2 Float)) e (V2 (Expr e Float)) where use = usePartsVec
instance Use (Var (V2 Int32)) e (V2 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V2 Bool)) e (V2 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V3 Float)) e (V3 (Expr e Float)) where use = usePartsVec
instance Use (Var (V3 Int32)) e (V3 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V3 Bool)) e (V3 (Expr e Bool)) where use = usePartsVec
instance Use (Var (V4 Float)) e (V4 (Expr e Float)) where use = usePartsVec
instance Use (Var (V4 Int32)) e (V4 (Expr e Int32)) where use = usePartsVec
instance Use (Var (V4 Bool)) e (V4 (Expr e Bool)) where use = usePartsVec

usePartsMat :: (Vector v, GLtype a, GLtype (v a)) => Var (v (v a)) -> v (v (Expr e a))
usePartsMat v = vecParts <$> vecParts (Expr $ varExpr v)

instance Use (Var (V2 (V2 Float))) e (V2 (V2 (Expr e Float))) where use = usePartsMat
instance Use (Var (V3 (V3 Float))) e (V3 (V3 (Expr e Float))) where use = usePartsMat
instance Use (Var (V4 (V4 Float))) e (V4 (V4 (Expr e Float))) where use = usePartsMat

instance (KnownNat s, GLtype a) => Use (Var (Arr s a)) e (Expr e (Arr s a)) where
	use = Expr . varExpr



instance Use (Var (Texture f)) e (Expr e (Texture f)) where
  use = Expr . varExpr
