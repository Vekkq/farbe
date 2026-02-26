{-# OPTIONS_GHC -fno-warn-tabs #-}

module Graphics.Farbe.ShaderCache where

import Graphics.Farbe.State
import Graphics.Farbe.BuildShader
import Graphics.Farbe.DMap

import Data.Hashable
import Control.Monad.IO.Class
import Control.Concurrent
import qualified Data.Map as M



instance Eq ExprI where
	(ExprI _ r ps) == (ExprI _ r2 ps2) = r == r2 && ps == ps2

instance Hashable ExprI where
	hashWithSalt salt (ExprI _ r ps) = salt `hashWithSalt` r `hashWithSalt` ps

-- ~ type Hash = Int

-- ~ addToCache :: (Farbe m, MonadIO m) => ExprI -> MVar (w ()) -> m ()
-- ~ addToCache e v = do
	-- ~ h <- snHash e
	-- ~ shaderCacheState (\(CacheState m1 m2)
		-- ~ -> ((),CacheState (M.insert h v m1) (M.insert (hash e) v m2)))

-- ~ snHash :: MonadIO m => a -> m Int
-- ~ snHash a = liftIO $ hashStableName <$> makeStableName a

-- ~ lookupCache :: (Farbe m, MonadIO m) => ExprI -> m (Maybe (MVar (w ())))
-- ~ lookupCache e = do
	-- ~ (CacheState m1 m2) <- shaderCacheGet
	-- ~ h <- snHash e
	-- ~ let mw = M.lookup h m1
	-- ~ let mw2 = M.lookup (hash e) m2
	-- ~ return $ listToMaybe $ catMaybes [mw, mw2]


