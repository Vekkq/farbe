{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Main (main) where

import Graphics.Farbe
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.OBJ

import Control.Monad

import Data.Maybe
import Data.Function

-- ~ import Shadow


frameShader :: Farbe m => [VArray (V2 Float)] -> m ()
frameShader = shader $ \(V2 x y) -> do
	-- ~ return (up 1 v', up 1 n' + texture (use t) (V2 1 (-1) * down fragCoord / 512))
	return (V4 x y 0.1 1, textureIO "test-resources/fish_red.jpg" (down fragCoord / 256))


renderFrame :: (MonadWindow m, Farbe m) => m ()
renderFrame = do
	frame <- newVArray $ [V2 (-1) 1, V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1), V2 1 (-1)]
	r <- newMat3
	fix $ \loop -> processEvents $ \es -> do
		updateRotate r
		frameShader [frame]
		anyMouseClick es $ renderobj
		loop



adjustZ (V4 x y z w) = V4 x y z' w
	where z' = z/10**3

basicShader :: Farbe m => Var Texture -> Var (Mat V3 V3 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
basicShader t r = shader $ \(n,v) -> do
	let v' = use r **| v
	n' <- transfer n
	return (adjustZ $ up 1 v', up 1 n' + texture (use t) (V2 1 (-1) * down fragCoord / 512))


renderbasic :: (MonadWindow m, Farbe m) => m ()
renderbasic = do
	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray
	cube <- readFileBinSTL "test-resources/cube1.stl" >>= newVArray

	t <- makeVarT =<< loadImage "test-resources/s0GpiMly_400x400.jpg"
	r <- newMat3

	fix $ \loop -> processEvents $ \es -> do
		updateRotate r
		basicShader t r [cube, teapot]
		anyMouseClick es $ renderFrame
		loop


shaderobj :: Farbe m
	=> Var (Mat V3 V3 Float) -> [VArray (V3 Float, V3 Float, V3 Float)] -> m ()
shaderobj r = shader $ \(v, t, n) -> do
	let v' = use r **| v
	t' <- transfer $ down t
	return (up 1 v', textureIO "test-resources/fish_red.jpg" t')

renderobj :: (MonadWindow m, Farbe m) => m ()
renderobj = do
	fishv <- loadOBJ "test-resources/fish_red.obj"
	fish <- newVArray $ map (\(OBJPoint v t n) -> (0.1 * v, t, n)) fishv
	r <- newMat3
	fix $ \loop -> processEvents $ \es -> do
		updateRotate r
		shaderobj r [fish]
		anyMouseClick es $ renderbasic
		loop


anyMouseClick es f = case es of
	[(EventMouseKey _ _ Down, _)] -> f
	_ -> return ()


persLightShader :: (MonadWindow m, Farbe m)
	=> Var (Mat V4 V4 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
persLightShader vm va = do
	($ va) $ shader $ \(n,v) -> do
		let v' = use vm **| fit 1 v
		vf <- transfer v'
		n' <- transfer $ use vm **| fit1 n
		let l = use vm **| fit1 lightPoint
		return (v', pure $ (*4) . (1.5-) $ vdistance (V4 0 0 (-1.9) 0) $ reflect (l-vf) n')

lightPoint :: V3 (Expr e Float)
lightPoint = V3 0 2 0


renderPers :: (MonadWindow m, Farbe m) => m ()
renderPers = do
	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray

	let floorFrame' = map (\v -> (V3 0 1 0, v)) $ floorFrame
	floorFrame'' <- newVArray floorFrame'
	-- ~ utah <- newVArray $ teapot ++ map (fmap (* V2 (-1) 1)) teapot
	let scene = undefined

	vm <- makeVarM4 =<< viewMat
	fix $ \loop -> processEvents $ \es -> do
		m <- viewMat
		swapVar vm m
		-- ~ t <- drawDepth $ lightShader [scene]
		persLightShader vm [floorFrame'']
		anyMouseClick es $ renderobj
		loop


newMat3 :: Farbe m => m (Var (Mat V3 V3 Float))
newMat3 = makeVarM3 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)


main :: IO ()
main = runFarbeT "" (InWindow (1000,800)) $ do
	-- ~ modifyConfig $ \f -> f { devDebugMode = True }
	renderPers


