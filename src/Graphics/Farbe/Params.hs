{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- ~ {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.Params where


import Graphics.Farbe.Shader
import Graphics.Farbe.State
import Graphics.Farbe.Attribute
import Graphics.Farbe.Vec
import Graphics.Farbe.Expr
import Graphics.Farbe.VertexArray
import Graphics.Farbe.Uniform



-- ~ class ShaderDefinition f g where
	-- ~ shade :: f -> FarbeT IO g

-- ~ instance (AttrType a b, Farbe m) =>
	-- ~ ShaderDefinition
		-- ~ (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
		-- ~ ([VArray a] -> m ())
	-- ~ where
	-- ~ shade f = return $ shader f

-- ~ instance (AttrType a b, ShaderDefinition f g, UploadDefault a, Use (Var a) V r, Has (FarbeT IO) g) =>
	-- ~ ShaderDefinition (r -> f) (a -> g) where
	-- ~ --shade :: (Expr e r -> f) -> m (a -> g)
	-- ~ shade f = do
		-- ~ v <- makeVarEmpty
		-- ~ g <- shade $ f $ use v
		-- ~ return $ \a -> liftF (swapVar v a) g


-- ~ colorful :: Mat V3 V3 Float -> FarbeT IO ([VArray (V3 Float, V3 Float)] -> FarbeT IO ())
-- ~ colorful = shade $ \r (n,v) -> colorful'
-- ~ colorful' :: Mat V3 V3 (Expr V Float) -> (V3 (Expr V Float), V3 (Expr V Float)) -> ShaderDefi
-- ~ colorful' r (n,v) = do
	-- ~ let v' = r **| v
	-- ~ n' <- transfer n
	-- ~ return (up 1 v', up 1 n' * 0.5 + 0.2)

-- ~ colorful :: Farbe m => Mat V3 V3 Float -> FarbeT IO ([VArray (V3 Float, V3 Float)] -> m ())
-- ~ colorful = shade $ \r (n,v) -> do
	-- ~ let v' = use r **| v
	-- ~ n' <- transfer n
	-- ~ return (up 1 v', up 1 n' * 0.5 + 0.2)

