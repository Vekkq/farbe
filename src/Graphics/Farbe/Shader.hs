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


import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Foldable
import Data.Array.IO
import Foreign hiding (void)
import Foreign.C


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
import Control.Applicative (Alternative)
import Control.Monad.RWS (RWST)

import GHC.TypeNats

import Debug.Trace



data Expr e a = Expr { unExpr :: ExprEnv } deriving Functor

data ExprEnv = ExprEnv { fnName :: Shdr String, rtype :: TypeS, fnAst :: [ExprEnv] }

data ExprS = ExprS String TypeS [ExprS]

type Shdr = BuildShaderT (ShaderEnvT IO)


runExprEnv :: ExprEnv -> Shdr ExprS
runExprEnv (ExprEnv m r ps) = do
	s <- m
	ps' <- mapM runExprEnv ps
	return $ ExprS s r ps'

newtype ShaderEnvT m a = ShaderEnvT { unShaderEnvT :: CounterT (DeferT (DeferT (HandTexT m))) a }
	deriving
		( Functor, Applicative, Monad, Alternative
		, MonadIO, Count
		)

instance MonadTrans ShaderEnvT where
	lift = ShaderEnvT . lift . lift . lift . lift

instance Monad m => Defer (DeferT (HandTexT m)) (ShaderEnvT m) where
	-- ~ defer :: DeferT m () -> ShaderEnvT m ()
	defer = ShaderEnvT . lift . defer

runShaderEnvT :: (HandTex m, MonadIO m) => ShaderEnvT IO a -> m (a, m ())
runShaderEnvT (ShaderEnvT m) = do
	(r,rm) <- f $ runDeferT $ runDeferT' $ runCounterT 1 m
	return (r, f rm)
	where
		f :: (HandTex m, MonadIO m) => HandTexT IO a -> m a
		f n = do
			t <- getTex
			(r,t') <- liftIO $ runHandTexT' t n
			setTex t'
			return r



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

#define SIMPLEFUNCTION_CLASSINSTANCES(fn,cn,op)                                    \
instance (cn m, Monad m) => cn (ReaderT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (WriterT w m) where { fn = lift op fn }  ;\
instance (cn m, Monad m) => cn (StateT r m) where { fn = lift op fn }             ;\
instance (cn m, Monad m) => cn (ContT r m) where { fn = lift op fn }              ;\
instance (cn m, Monad m) => cn (ExceptT r m) where { fn = lift op fn }            ;\
instance (cn m, Monad m, Monoid w) => cn (RWST r w s m) where { fn = lift op fn } ;\

SIMPLEFUNCTION_CLASSINSTANCES(buildShaderState,BuildShader,.)

addHeader :: (GLtype a, BuildShader m) => String -> a -> String -> m ()
addHeader i a n = buildShaderState $ \s -> ((),) $
		s { header = S.insert (unwords [i, slNameWithPrec a, n, ";"]) $ header s }


addExpr :: String -> Expr e a -> Shdr ()
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

class (MonadIO m, Defer (DeferT (HandTexT IO)) m) => PostShader m
instance (MonadIO m, Defer (DeferT (HandTexT IO)) m) => PostShader m




compile :: (MonadIO m, HandTex m, AttrType a b)
	=> (b -> DeferT Shdr (V4 (Expr V Float), V4 (Expr F Float)))
	-> m ([VArray a] -> m ())
compile f = do
	sp <- glCreateProgram
	(vao,exec) <- runShaderEnvT $
		join $ addShader sp GL_VERTEX_SHADER $ do
			(i,e) <- setAttributes (err :: a)
			((vs,fs),fm) <- runDeferT $ f e
			addExpr "gl_Position" $ exprVec vs
			return $ addShader sp GL_FRAGMENT_SHADER $ do
				addExpr "gl_FragColor" $ exprVec $ fs
				fm
				return i
	return $ \varrs -> do
		liftIO $ putStrLn "draw"
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
		++ toCExpr' (bexpr st)
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


toCExpr' :: [(String, ExprS)] -> String
toCExpr' xs = unlines $ reverse $ for xs $ \(s,x) -> s ++ " = " ++ toCExpr x ++";"


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

-- a -> BuildShaderT (ShaderEnvT m) (GLuint, b)
-- | Make VAO
setAttributes :: (AttrType a b, BuildShader m, Count m, PostShader m) => a -> m (Vao, b)
-- ~ setAttributes = undefined
setAttributes a = do
	i <- glGenVertexArray
	glBindVertexArray i
	e <- evalStateT (unAttrib $ setAttribute a) 0
	return (i, e)


setupAttribute1
	:: (GLtype a, Storable a, Attrib m, BuildShader m, Count m, PostShader m)
	=> a
	-> m (Expr V a)
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
	return $ (liftExprShdr' $ runOnce initExpr)

class Storable a => AttrType a b | a -> b, b -> a where
	setAttribute :: (Attrib m, BuildShader m, Count m, PostShader m) => a -> m b


instance AttrType Bool (Expr V Bool) where setAttribute = setupAttribute1
instance AttrType Int32 (Expr V Int32) where setAttribute = setupAttribute1
instance AttrType Float (Expr V Float) where setAttribute = setupAttribute1

instance AttrType (Normalized Float) (Normalized (Expr V Float)) where
	setAttribute a = fmap Normalized $ fmap2 unNormalized $ setupAttribute1 a
		where
		fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
		fmap2 f = fmap (fmap f)


instance (AttrType a c, AttrType b d) => AttrType (a,b) (c,d) where
	setAttribute _ = liftM2 (,) (setAttribute (err :: a)) (setAttribute (err :: b))

instance (AttrType a x, AttrType b y, AttrType c z) => AttrType (a,b,c) (x,y,z) where
	setAttribute _ = liftM3 (,,)
		(setAttribute (err :: a))
		(setAttribute (err :: b))
		(setAttribute (err :: c))

instance (AttrType a x, AttrType b y, AttrType c z, AttrType d w) =>
	AttrType (a,b,c,d) (x,y,z,w) where
	setAttribute _ = liftM4 (,,,)
		(setAttribute (err :: a))
		(setAttribute (err :: b))
		(setAttribute (err :: c))
		(setAttribute (err :: d))

attribPartsVec
	:: (GLtype a, GLtype (v a), Storable a, Storable (v a), Vector v, Attrib m, BuildShader m, Count m, PostShader m)
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
	:: (Attrib m, GLtype (v (v a)), GLtype (v a), GLtype a, Storable (v (v a)), Vector v, BuildShader m, Count m, PostShader m)
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


liftExpr :: (GLtype a) => String -> [ExprEnv] -> Expr e a
liftExpr s p = liftExprShdr (return s) p

liftExpr' :: (GLtype a) => String -> Expr e a
liftExpr' s = liftExpr s []

liftExprShdr :: forall e a . (GLtype a) => Shdr String -> [ExprEnv] -> Expr e a
liftExprShdr s p = Expr $ ExprEnv s (toTypeS (err :: a)) p

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

-- ~ instance LiftExpr (a -> b) r where
	-- ~ liftE f = (\a -> (a:))



-- ~ liftE4 :: String -> Expr e a1 -> Expr e a2 -> Expr e a3 -> Expr e a4 -> Expr e a5
-- ~ liftE4 s (Expr a) (Expr b) (Expr c) (Expr d) = Expr $ Fn s [a,b,c,d]


vecParts :: (GLtype a, Vector v) => Expr e (v a) -> v (Expr e a)
vecParts e = fromListFill err $ map (\i -> arrV e i) $ map expr [0..]

exprVec :: forall e a v . (GLtype a, Vector v, GLtype (v a)) => v (Expr e a) -> Expr e (v a)
exprVec v = liftExpr (slName (err :: v a)) $ map unExpr $ toList v

exprMat :: forall a e v .(GLtype a, Vector v, GLtype (v a), GLtype (v (v a)))
	=> v (v (Expr e a)) -> Expr e (v (v a))
exprMat v = liftExpr (slName (err :: v a)) $ map unExpr $ concatMap toList $ toList v

arrV :: (GLtype a, Vector v) => Expr e (v a) -> Expr e Int32 -> Expr e a
arrV = liftE2 "[]"


name :: (Count m, GLtype a) => String -> a -> m String
name s a = generateName $ s ++ glShortName a

withString :: MonadIO m => String -> (CString -> IO a) -> m a
withString n f = liftIO $ bracket (newCAString n) free f



-- Shader value transfer -----------------------------------------------------------------

-- | Transfer values from vertex shader to fragment shader. Floating point numbers will be interpolated among its triangle space. Integers are taken from the first point of the triangle.

class Transfer a b | a -> b, b -> a where
	transfer :: a -> DeferT Shdr b

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


deriving instance (Monad m, BuildShader m) => BuildShader (DeferT m)

transfer1 :: forall a . GLtype a => Expr V a -> DeferT Shdr (Expr F a)
transfer1 e = do
		let a = err :: a
		n <- name "t" a
		lift $ addExpr n e
		addHeader "varying" a $ n
		defer $ do
			addHeader "in" a n
		return $ liftExprShdr' $ return n



-- Uniform variables ---------------------------------------------------------------------

data Var a = Var { varExpr :: ExprEnv, varMVar :: MVar a }

swapVar :: MonadIO m => Var a -> a -> m a
swapVar v = liftIO . swapMVar (varMVar v)

readVar :: MonadIO m => Var a -> m a
readVar = liftIO . readMVar . varMVar


makeVar :: forall a m . (Count m, MonadIO m, GLtype a, Upload a) => a -> m (Var a)
makeVar a = do
	m <- liftIO $ newMVar a
	vname <- (name "u" a)
	let r = do
		addHeader "uniform" a vname
		s <- getShader
		defer $ do
			l <- withString vname $ glGetUniformLocation s
			wc <- makeRunWhenChanged $ upload l
			defer $ (liftIO $ readMVar m) >>= runwc wc
		return vname
	return $ Var (ExprEnv r (toTypeS (err :: a)) []) m


class (GLtype a, Eq a) => Upload a where
	upload :: (MonadIO m, HandTex m) => GLint -> a -> m ()

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
		when (i /= i') $ do
			glActiveTexture $ GL_TEXTURE0 + u'
			glBindTexture GL_TEXTURE_2D i
			glUniform1i l $ itoi u'
			liftIO $ swapMVar mu u'
			u'' <- succU ts u'
			liftIO $ writeArray ts u'' i
			setTex $ TexState u'' ts
		where
		succU ts x = do
			let x' = succ x
			(a,b) <- liftIO $ getBounds ts
			return $ if x' >= b then a else x'

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
