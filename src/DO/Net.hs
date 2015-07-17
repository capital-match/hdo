{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Interpreter for accessing DO API through the web using [wreq http://www.serpentine.com/wreq].
module DO.Net(mkDOClient) where

import           Control.Applicative
import           Control.Comonad.Env.Class    (ComonadEnv, ask)
import           Control.Comonad.Trans.Cofree (CofreeT, coiterT)
import           Control.Comonad.Trans.Env    (Env, env)
import           Control.Lens                 ((&), (.~))
import           Data.Aeson                   as A
import qualified Data.Aeson.Types             as A
import           Data.ByteString.Char8        (pack)
import qualified Data.HashMap.Strict          as H
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Vector                  as V
import           Network.URI                  (URI, parseURI)
import           Network.Wreq
import           Prelude                      as P

import           Commands
import           DigitalOcean                 as DO hiding (URI)
import           Net

rootURI :: String
rootURI = "https://api.digitalocean.com"

apiVersion ::  String
apiVersion = "v2"

dropletsURI :: String
dropletsURI = "droplets"

imagesURI :: String
imagesURI = "images"

keysURI :: String
keysURI = "keys"

sizesURI :: String
sizesURI = "sizes"

accountURI :: String
accountURI = "account"

dropletsEndpoint :: String
dropletsEndpoint = rootURI </> apiVersion </> dropletsURI

keysEndpoint :: String
keysEndpoint = rootURI </> apiVersion </> accountURI </> keysURI

sizesEndpoint :: String
sizesEndpoint = rootURI </> apiVersion </> sizesURI

imagesEndpoint :: String
imagesEndpoint = rootURI </> apiVersion </> imagesURI

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

doList :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> (NetT m [Droplet], w a)
doList w = maybe (return [], w)
           (\ t -> let droplets = toList "droplets" <$> getJSONWith (authorisation t) (toURI dropletsEndpoint)
                   in (droplets, w))
           (authToken (ask w))

doListKeys :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> (NetT m [Key], w a)
doListKeys w = maybe (return [], w)
               (\ t -> let droplets = toList "ssh_keys" <$> getJSONWith (authorisation t) (toURI keysEndpoint)
                       in (droplets, w))
               (authToken (ask w))

doListSizes :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> (NetT m [Size], w a)
doListSizes w = maybe (return [], w)
               (\ t -> let sizes = toList "sizes" <$> getJSONWith (authorisation t) (toURI sizesEndpoint)
                       in (sizes, w))
               (authToken (ask w))

doListImages :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> (NetT m [Image], w a)
doListImages w = maybe (return [], w)
                 (\ t -> let droplets = toList "images" <$> getJSONWith (authorisation t) (toURI imagesEndpoint)
                         in (droplets, w))
                 (authToken (ask w))

dropletFromResponse :: Either String Value -> Either String Droplet
dropletFromResponse (Right (Object b)) = A.parseEither parseJSON (b H.! "droplet")
dropletFromResponse v                  = Left $ "cannot decode JSON value to a droplet " ++ show v

doCreate :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> BoxConfiguration -> (NetT m (Either String Droplet), w a)
doCreate w config = maybe (return $ Left "no authentication token defined", w)
                    (\ t -> let opts = (authorisation t)
                                droplets = postJSONWith opts (toURI dropletsEndpoint) (toJSON config)
                                           >>= (\ d -> case dropletFromResponse d of
                                                        Right b -> waitForBoxToBeUp opts 60 b
                                                        err     -> return err)
                            in (droplets, w))
                    (authToken (ask w))

doDestroyDroplet :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (NetT m (Maybe String), w a)
doDestroyDroplet w dropletId = maybe (return $ Just "no authentication token defined", w)
                               (\ t -> let r = deleteJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId) >> return Nothing
                                       in (r, w))
                               (authToken (ask w))

actionResult :: Either String Value -> Either String ActionResult
actionResult (Right (Object r)) = A.parseEither parseJSON (r H.! "action")
actionResult e                  = Left $ "cannot extract action result from " ++ show e

doAction :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> Action -> (NetT m (Either String ActionResult), w a)
doAction w dropletId action = maybe (return $ Left "no authentication token defined", w)
                              (\ t -> let r = postJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "actions") (toJSON action)
                                              >>= return . actionResult
                                      in (r, w))
                              (authToken (ask w))

waitForBoxToBeUp :: (Monad m) => Options -> Int -> Droplet -> NetT m (Either String Droplet)
waitForBoxToBeUp _    0 box  = return (Right box)
waitForBoxToBeUp opts n box  = do
  waitFor 1000000 ("waiting for droplet " ++ name box ++ " to become Active: " ++ show (n) ++ "s")
  b <- getJSONWith opts (toURI $ dropletsEndpoint </> show (DO.id box))
  case dropletFromResponse (Right b) of
   Right box'-> if status box' == Active
                then return (Right box')
                else waitForBoxToBeUp opts (n-1) box'
   err       -> return $ err

mkDOClient :: (Monad m) => ToolConfiguration -> CofreeT (CoDO (NetT m)) (Env ToolConfiguration) (NetT m ())
mkDOClient config = coiterT next start
  where
    next = CoDO
           <$> doList
           <*> doCreate
           <*> doDestroyDroplet
           <*> doAction
           <*> doListKeys
           <*> doListSizes
           <*> doListImages
    start = env config (return ())
