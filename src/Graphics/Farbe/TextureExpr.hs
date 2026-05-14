{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}

module Graphics.Farbe.TextureExpr where

import Graphics.Farbe.Vec
import Graphics.Farbe.Expr
import Graphics.Farbe.Texture
import Graphics.Farbe.JuicyPixels
import Graphics.Farbe.BuildShader
import Graphics.Farbe.ShaderEnv
import Graphics.Farbe.Uniform
import Graphics.Farbe.GL
import Graphics.Farbe.Name
import Graphics.Farbe.Utility

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Graphics.GL.Embedded20
import Graphics.GL.Types

#define bottom undefined

-- ~ textureIO :: V2 (Expr e Float) -> String -> V4 (Expr e Float)
-- ~ textureIO p s = flip texture p $ Expr $ ExprI shdr TTex []
	-- ~ where
		-- ~ vname = sani s
		-- ~ a = bottom :: Texture RGB
		-- ~ shdr = do
			-- ~ t :: Texture RGB <- loadImage' s
			-- ~ let t' = t { path = s }
			-- ~ b <- addHeader "uniform" a $ vname
			-- ~ s <- getShaderId
			-- ~ m <- liftIO $ newMVar a
			-- ~ when b $ postShader $ do
				-- ~ l <- withString vname $ glGetUniformLocation s
				-- ~ wc <- makeRunWhenChanged $ upload l
				-- ~ -- RunWhenChanged will bork for textures, since they need to be always checked for assigned tex unit
				-- ~ preRender $ do
					-- ~ (liftIO $ readMVar m) >>= runwc wc
					-- ~ return True
			-- ~ return vname


sani :: String -> String
sani = id -- TODO
