{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- | Interpreter for accessing DO API through the web using [wreq http://www.serpentine.com/wreq].
module Network.DO.Net(mkDOClient) where

import           Control.Applicative
import           Control.Comonad.Env.Class    (ComonadEnv)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (Env, env)
import           Control.Monad.Trans          (MonadIO)
import           Data.Functor.Product
import           Data.Proxy
import           Prelude                      as P

import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Net
import           Network.DO.IP
import           Network.DO.Net.Common
import           Network.DO.Pairing
import           Network.DO.Types             as DO hiding (URI)
import           Network.REST

imagesURI :: String
imagesURI = "images"

keysURI :: String
keysURI = "keys"

sizesURI :: String
sizesURI = "sizes"

accountURI :: String
accountURI = "account"

regionsURI :: String
regionsURI = "regions"

keysEndpoint :: String
keysEndpoint = rootURI </> apiVersion </> accountURI </> keysURI

sizesEndpoint :: String
sizesEndpoint = rootURI </> apiVersion </> sizesURI

imagesEndpoint :: String
imagesEndpoint = rootURI </> apiVersion </> imagesURI

regionsEndpoint :: String
regionsEndpoint = rootURI </> apiVersion </> regionsURI

instance Listable Key where
  listEndpoint _ = keysEndpoint
  listField _    = "ssh_keys"

instance Listable Size where
  listEndpoint _ = sizesEndpoint
  listField _    = "sizes"

instance Listable Image where
  listEndpoint _ = imagesEndpoint
  listField _    = "images"

instance Listable Region where
  listEndpoint _ = regionsEndpoint
  listField _    = "regions"

genericCommands :: (Monad m, ComonadEnv ToolConfiguration w) => w a -> CoDO (RESTT m) (w a)
genericCommands = CoDO
                  <$> queryList (Proxy :: Proxy Key)
                  <*> queryList (Proxy :: Proxy Size)
                  <*> queryList (Proxy :: Proxy Image)
                  <*> queryList (Proxy :: Proxy Region)

mkDOClient :: (MonadIO m) => ToolConfiguration
           -> CofreeT (CoDO (RESTT m) :*: CoDropletCommands (RESTT m) :*: CoIPCommands (RESTT m)) (Env ToolConfiguration) (RESTT m ())
mkDOClient config = coiterT next start
  where
    next = Pair
           <$> genericCommands
           <*> (Pair
                 <$> dropletCommandsInterpreter
                 <*> ipCommandsInterpreter
               )
    start = env config (return ())
