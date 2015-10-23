{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Network interpreter for Droplets specific API
module Network.DO.Droplets.Net(dropletCommandsInterpreter) where

import           Control.Applicative
import           Control.Comonad.Env.Class    (ComonadEnv, ask)
import           Data.Aeson                   as A
import qualified Data.Aeson.Types             as A
import qualified Data.HashMap.Strict          as H
import           Data.Maybe
import           Data.Proxy
import           Network.Wreq                 hiding (Proxy)
import           Prelude                      as P

import           Network.DO.Droplets.Commands
import           Network.DO.Net.Common
import           Network.DO.Types             as DO hiding (URI)
import           Network.REST

dropletsURI :: String
dropletsURI = "droplets"

dropletsEndpoint :: String
dropletsEndpoint = rootURI </> apiVersion </> dropletsURI

instance Listable Droplet where
  listEndpoint _ = dropletsEndpoint
  listField _    = "droplets"

doListSnapshots :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (NetT m [Image], w a)
doListSnapshots w dropletId =
  maybe (return [], w)
  (\ t -> let snapshots = toList "snapshots" <$> getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "snapshots")
          in (snapshots, w))
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

doGetAction :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> Id -> (NetT m (Either String ActionResult), w a)
doGetAction w dropletId actionId = maybe (return $ Left "no authentication token defined", w)
                                   (\ t -> let r = getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "actions" </> show actionId)
                                                   >>= return . actionResult . Right
                                           in (r, w))
                                   (authToken (ask w))

doShowDroplet  :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (NetT m (Either String Droplet), w a)
doShowDroplet w dropletId = maybe (return $ Left "no authentication token defined", w)
                            (\ t -> let r = dropletFromResponse . Right <$> getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId)
                                    in (r, w))
                            (authToken (ask w))

waitForBoxToBeUp :: (Monad m) => Options -> Int -> Droplet -> NetT m (Either String Droplet)
waitForBoxToBeUp _    0 box  = return (Right box)
waitForBoxToBeUp opts n box  = do
  waitFor 1000000 ("waiting for droplet " ++ name box ++ " to become Active: " ++ show (n) ++ "s")
  b <- getJSONWith opts (toURI $ dropletsEndpoint </> show (dropletId box))
  case dropletFromResponse (Right b) of
   Right box'-> if status box' == Active
                then return (Right box')
                else waitForBoxToBeUp opts (n-1) box'
   err       -> return $ err

dropletCommandsInterpreter :: (Monad m, ComonadEnv ToolConfiguration w) => w a -> CoDropletCommands (NetT m) (w a)
dropletCommandsInterpreter = CoDropletCommands
                             <$> queryList (Proxy :: Proxy Droplet)
                             <*> doCreate
                             <*> doDestroyDroplet
                             <*> doAction
                             <*> doGetAction
                             <*> doListSnapshots
                             <*> doShowDroplet

