


class ShaderCache m where
	shader :: (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float))) -> m (MVar Shader)


-- ~ render :: (MonadIO m, HandTex m, AttrType a b)
	-- ~ => (b -> ShaderM (V4 (Expr V Float), V4 (Expr F Float)))
	-- ~ -> ([VArray a] -> Render m)
-- ~ render = undefined
