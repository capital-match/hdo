{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Network interpreter for Floating IPs specific API
module Network.DO.IP.Net(ipCommandsInterpreter) where

import           Control.Applicative
import           Control.Comonad.Env.Class (ComonadEnv, ask)
import           Control.Exception         (IOException)
import           Control.Monad.Trans       (MonadIO)
import           Data.Aeson                as A hiding (Result)
import qualified Data.Aeson.Types          as A
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Proxy
import           Network.DO.IP.Commands
import           Network.DO.Net.Common
import           Network.DO.Types          as DO hiding (URI)
import           Network.REST
import           Prelude                   as P hiding (error)

floatingIpsURI :: String
floatingIpsURI = "floating_ips"

floatingIpsEndpoint :: String
floatingIpsEndpoint = rootURI </> apiVersion </> floatingIpsURI

instance Listable FloatingIP where
  listEndpoint _ = floatingIpsEndpoint
  listField _    = "floating_ips"

doCreateIP :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> FloatingIPTarget -> (RESTT m (Result FloatingIP), w a)
doCreateIP w config = maybe (return $ error "no authentication token defined", w)
  runQuery
  (authToken (ask w))
  where
    runQuery t = let opts             = authorisation t
                     ip               = postJSONWith opts (toURI floatingIpsEndpoint) (toJSON config) >>= return . fromResponse "floating_ip"
                 in (ip, w)

ipCommandsInterpreter :: (MonadIO m, ComonadEnv ToolConfiguration w) => w a -> CoIPCommands (RESTT m) (w a)
ipCommandsInterpreter = CoIPCommands
                        <$> queryList (Proxy :: Proxy FloatingIP)
                        <*> doCreateIP

