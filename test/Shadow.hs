{-# OPTIONS_GHC -fno-warn-tabs #-}

module Shadow where

import Graphics.Farbe
import Graphics.Farbe.Window hiding (processEvents)
import Graphics.Farbe.STL
import Graphics.Farbe.JuicyPixels

import Control.Monad
import Data.Maybe
import Data.Function


lightShader :: Farbe m => Var Texture -> [VArray (V3 Float, V3 Float)] -> m ()
lightShader t = shader $ \(n,v) -> return (fit1 $ pitch (pi/2) v,1)


shadowShader :: (MonadWindow m, Farbe m)
	=> Var (Mat V4 V4 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
shadowShader vm va = do
	-- ~ dim <- windowDim0to1
	($ va) $ shader $ \(n,v) -> do
		let v' = use vm **| fit 1 v
		vf <- transfer v'
		n' <- transfer $ use vm **| fit1 n
		let l = use vm **| fit1 lightPoint
		return (v', pure $ (*4) . (1.5-) $ vdistance (V4 0 0 (-1.9) 0) $ reflect (l-vf) n')
-- ~ pure $ (1.5-) $  vdistance (V3 0 0 (-2)) $ reflect (l-vf) n')

-- | light source,
-- ~ lightIntensity :: V3 a -> V3 a -> a
-- ~ lightIntensity l = (1.5-) . vdistance (V3 0 0 (-1)) . reflect l


lightPoint :: V3 (Expr e Float)
lightPoint = V3 0 2 0


renderShadow :: (MonadWindow m, Farbe m) => m ()
renderShadow = do
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
		shadowShader vm [floorFrame'']
		-- ~ anyMouseClick es $ renderFrame r
		loop








