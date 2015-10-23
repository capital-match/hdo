{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Interpreter for accessing DO API through the web using [wreq http://www.serpentine.com/wreq].
module Network.DO.Net(mkDOClient) where

import           Control.Applicative
import           Control.Comonad.Env.Class    (ComonadEnv, ask)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (Env, env)
import           Control.Lens                 ((&), (.~))
import           Data.Aeson                   as A
import qualified Data.Aeson.Types             as A
import           Data.ByteString.Char8        (pack)
import           Data.Functor.Product
import qualified Data.HashMap.Strict          as H
import           Data.Maybe
import           Data.Monoid                  hiding (Product)
import           Data.Proxy
import           Data.Text                    (Text)
import qualified Data.Vector                  as V
import           Network.URI                  (URI, parseURI)
import           Network.Wreq                 hiding (Proxy)
import           Prelude                      as P

import           Network.DO.Commands
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Net
import           Network.DO.Net.Common
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

keysEndpoint :: String
keysEndpoint = rootURI </> apiVersion </> accountURI </> keysURI

sizesEndpoint :: String
sizesEndpoint = rootURI </> apiVersion </> sizesURI

imagesEndpoint :: String
imagesEndpoint = rootURI </> apiVersion </> imagesURI


instance Listable Key where
  listEndpoint _ = keysEndpoint
  listField _    = "ssh_keys"

instance Listable Size where
  listEndpoint _ = sizesEndpoint
  listField _    = "sizes"

instance Listable Image where
  listEndpoint _ = imagesEndpoint
  listField _    = "images"

genericCommands :: (Monad m, ComonadEnv ToolConfiguration w) => w a -> CoDO (NetT m) (w a)
genericCommands = CoDO
                  <$> queryList (Proxy :: Proxy Key)
                  <*> queryList (Proxy :: Proxy Size)
                  <*> queryList (Proxy :: Proxy Image)

mkDOClient :: (Monad m) => ToolConfiguration -> CofreeT (Product (CoDO (NetT m)) (CoDropletCommands (NetT m))) (Env ToolConfiguration) (NetT m ())
mkDOClient config = coiterT next start
  where
    next = Pair
           <$> genericCommands
           <*> dropletCommandsInterpreter
    start = env config (return ())
