{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.DO.Net.Common where

import           Control.Comonad.Env.Class (ComonadEnv, ask)
import           Control.Lens              ((&), (.~))
import           Data.Aeson                as A
import qualified Data.Aeson.Types          as A
import           Data.ByteString.Char8     (pack)
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           Network.DO.Types          as DO hiding (URI)
import           Network.REST
import           Network.URI               (URI, parseURI)
import           Network.Wreq              hiding (Proxy)
import           Prelude                   as P

rootURI :: String
rootURI = "https://api.digitalocean.com"

apiVersion ::  String
apiVersion = "v2"

(</>) :: String -> String -> String
s </> ('/': s') = s ++ s'
s </> s'        = s ++ "/" ++ s'

toURI :: String -> URI
toURI = fromJust . parseURI

toList :: (FromJSON a) => Text -> Value -> [a]
toList k (Object o) = let Array boxes = o  H.! k
                      in mapMaybe (A.parseMaybe parseJSON) (V.toList boxes)
toList _  _         = []

authorisation :: String -> Options
authorisation t = defaults & header "Authorization" .~ ["Bearer " <> pack t]

class Listable a where
  listEndpoint :: Proxy a -> String
  listField :: Proxy a -> Text

queryList :: (ComonadEnv ToolConfiguration w, Monad m, Listable b, FromJSON b) => Proxy b -> w a -> (NetT m [b], w a)
queryList p w = maybe (return [], w)
                (\ t -> let droplets = toList (listField p) <$> getJSONWith (authorisation t) (toURI (listEndpoint p))
                        in (droplets, w))
                (authToken (ask w))

