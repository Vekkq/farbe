
module Graphics.Farbe.DChan where

import Control.Monad (void)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.IO.Class



-- | Dumbchan - implementation of Chan using a single MVar

newtype DChan a = DChan { dcVar :: MVar a }

newDChan :: MonadIO m => m (DChan a)
newDChan = liftIO $ fmap DChan newEmptyMVar

readDChan :: MonadIO m => DChan a -> m a
readDChan = liftIO . takeMVar . dcVar

writeDChan :: MonadIO m => DChan a -> a -> m ()
writeDChan d a = liftIO . void . forkIO $ putMVar (dcVar d) a

tryReadDChan :: MonadIO m => DChan a -> m (Maybe a)
tryReadDChan = liftIO . tryTakeMVar . dcVar

infixl 8 .:
(.:) = (.).(.)
