{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Network.DO.Net.Common where

import           Control.Comonad.Env.Class (ComonadEnv, ask)
import           Data.Aeson                as A hiding (Result)
import qualified Data.Aeson.Types          as A
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           Network.DO.Types          as DO hiding (URI, error)
import           Network.REST
import           Network.URI               (URI, parseURI)
import           Prelude                   as P

rootURI :: String
rootURI = "https://api.digitalocean.com"

apiVersion ::  String
apiVersion = "v2"

(</>) :: String -> String -> String
s </> ('/': s') = s ++ s'
s </> s'        = s ++ "/" ++ s'

toURI :: String -> URI
toURI s = maybe (P.error $ "cannot parse URI from " ++ s) id $ parseURI s

toList :: (FromJSON a) => Text -> Value -> [a]
toList k (Object o) = let Array boxes = o  H.! k
                      in mapMaybe (A.parseMaybe parseJSON) (V.toList boxes)
toList _  _         = []

class Listable a where
  listEndpoint :: Proxy a -> String
  listField :: Proxy a -> Text

queryList :: (ComonadEnv ToolConfiguration w, Monad m, Listable b, FromJSON b) => Proxy b -> w a -> (RESTT m [b], w a)
queryList p w = maybe (return [], w)
                (\ t -> let droplets = toList (listField p) <$> getJSONWith (authorisation t) (toURI (listEndpoint p))
                        in (droplets, w))
                (authToken (ask w))

-- |Extract a typed result from a JSON output
fromResponse :: (FromJSON a) => Text -> Either String Value -> Result a
fromResponse key (Right (Object b)) = either error (Right . id) $ A.parseEither parseJSON (b H.! key)
fromResponse _   v                  = error $ "cannot decode JSON value to a FloatingIP " ++ show v
