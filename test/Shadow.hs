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
	=> Var (Mat V3 V3 Float) -> [VArray (V3 Float, V3 Float)] -> m ()
shadowShader r va = do
	dim <- windowDim0to1
	($ va) $ shader $ \(n,v) -> do
		let v' = use r **| v
		vf <- transfer v'
		n' <- transfer $ use r **| n
		-- ~ l' <- transfer l
		let l = use r **| lightPoint
		return (adjustZ $ up 1 v', pure $ (1.5-) $  vdistance (V3 0 0 (-2)) $ reflect (l-vf) n')


-- | light source,
-- ~ lightIntensity :: V3 a -> V3 a -> a
-- ~ lightIntensity l = (1.5-) . vdistance (V3 0 0 (-1)) . reflect l


lightPoint :: V3 (Expr e Float)
lightPoint = V3 0 1 0

adjustZ (V4 x y z w) = V4 x y z' w
	where z' = z/10**3

renderShadow :: (MonadWindow m, Farbe m) => Var (Mat V3 V3 Float) -> m ()
renderShadow r = do
	teapot <- readFileBinSTL "test-resources/teapot1.stl" >>= newVArray

	let floorFrame' = map (\v -> (V3 0 1 0, v)) $ floorFrame
	floorFrame'' <- newVArray floorFrame'
	-- ~ utah <- newVArray $ teapot ++ map (fmap (* V2 (-1) 1)) teapot
	let scene = undefined

	fix $ \loop -> processEvents $ \es -> do
		updateRotate es r
		-- ~ t <- drawDepth $ lightShader [scene]
		shadowShader r [floorFrame'']
		-- ~ anyMouseClick es $ renderFrame r
		loop








