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
import           Control.Monad.Trans       (MonadIO)
import           Data.Aeson                as A hiding (Result)
import           Data.IP
import           Data.Maybe
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

doDeleteIP :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> IP -> (RESTT m (Maybe String), w a)
doDeleteIP w ip = maybe (return $ Just "no authentication token defined", w)
                  (\ t -> let r = deleteJSONWith (authorisation t) (toURI $ floatingIpsEndpoint </> show ip) >> return Nothing
                          in (r, w))
                  (authToken (ask w))

doAction :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> IP -> IPAction -> (RESTT m (Result (ActionResult IPActionType)), w a)
doAction w ip action = maybe (return $ error "no authentication token defined", w)
                       (\ t -> let r = postJSONWith (authorisation t) (toURI $ floatingIpsEndpoint </> show ip </> "actions") (toJSON action)
                                       >>= return . fromResponse "action"
                               in (r, w))
                       (authToken (ask w))

ipCommandsInterpreter :: (MonadIO m, ComonadEnv ToolConfiguration w) => w a -> CoIPCommands (RESTT m) (w a)
ipCommandsInterpreter = CoIPCommands
                        <$> queryList (Proxy :: Proxy FloatingIP)
                        <*> doCreateIP
                        <*> doDeleteIP
                        <*> doAction

