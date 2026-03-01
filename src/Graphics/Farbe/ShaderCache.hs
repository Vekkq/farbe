{-# OPTIONS_GHC -fno-warn-tabs #-}

module Graphics.Farbe.ShaderCache where

import Graphics.Farbe.Vec
import Graphics.Farbe.State
import Graphics.Farbe.BuildShader

import Data.Hashable
import Control.Monad.IO.Class
import Control.Concurrent
-- ~ import qualified Data.DMap as D




instance Eq ExprI where
	(ExprI _ r ps) == (ExprI _ r2 ps2) = r == r2 && ps == ps2

instance Eq (Expr e a) where
	Expr i == Expr i2 = i == i2

instance Hashable ExprI where
	hashWithSalt salt (ExprI _ r ps) = salt `hashWithSalt` r `hashWithSalt` ps

instance Hashable (Expr e a) where
	hashWithSalt salt (Expr x) = hashWithSalt salt x

instance (Hashable a) => Hashable (V4 a) where
	hashWithSalt salt = foldl hashWithSalt salt

-- ~ type Hash = Int

-- ~ lookupCache :: (Farbe m, MonadIO m) => ExprI -> m (Maybe ShExec)
-- ~ lookupCache e = do
	-- ~ d <- getShaderCache
	-- ~ D.lookup e d
