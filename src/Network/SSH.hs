module Network.SSH
       (ssh)
       where

import           Control.Exception   (Exception, try)
import           Control.Monad.Trans (MonadIO (..))
import           System.Process      (callProcess)

ssh :: (MonadIO m, Exception e) => [String] -> m (Either e ())
ssh args = liftIO $ try (callProcess "ssh" args)
