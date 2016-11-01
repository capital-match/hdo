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
import           Control.Exception            (IOException)
import           Control.Monad.Trans          (MonadIO)
import           Data.Aeson                   as A hiding (Result)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.Proxy
import           Network.DO.Droplets.Commands
import           Network.DO.Droplets.Utils
import           Network.DO.Net.Common
import           Network.DO.Types             as DO hiding (URI)
import           Network.REST
import           Network.Wreq                 hiding (Proxy)
import           Prelude                      as P hiding (error)

dropletsURI :: String
dropletsURI = "droplets"

dropletsEndpoint :: String
dropletsEndpoint = rootURI </> apiVersion </> dropletsURI

instance Listable Droplet where
  listEndpoint _ = dropletsEndpoint
  listField _    = "droplets"

doListSnapshots :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (RESTT m [Image], w a)
doListSnapshots w dropletId =
  maybe (return [], w)
  (\ t -> let snapshots = toList "snapshots" <$> getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "snapshots")
          in (snapshots, w))
  (authToken (ask w))

doCreate :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> BoxConfiguration -> (RESTT m (Result Droplet), w a)
doCreate w config = maybe (return $ error "no authentication token defined", w)
  runQuery
  (authToken (ask w))
  where
    runQuery t = let opts             = authorisation t
                     droplets         = postJSONWith opts (toURI dropletsEndpoint) (toJSON config) >>= handleResponse
                     handleResponse d = case fromResponse "droplet" d of
                       Right b  -> if (not $ backgroundCreate config)
                                  then waitForBoxToBeUp opts 60 b
                                  else return (Right b)
                       err      -> return err
                 in (droplets, w)

doDestroyDroplet :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (RESTT m (Maybe String), w a)
doDestroyDroplet w dropletId = maybe (return $ Just "no authentication token defined", w)
                               (\ t -> let r = deleteJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId) >> return Nothing
                                       in (r, w))
                               (authToken (ask w))

doAction :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> Action -> (RESTT m (Result (ActionResult DropletActionType)), w a)
doAction w dropletId action = maybe (return $ error "no authentication token defined", w)
                              (\ t -> let r = postJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "actions") (toJSON action)
                                              >>= return . fromResponse "action"
                                      in (r, w))
                              (authToken (ask w))

doGetAction :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> Id -> (RESTT m (Result (ActionResult DropletActionType)), w a)
doGetAction w dropletId actionId = maybe (return $ error "no authentication token defined", w)
                                   (\ t -> let r = getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId </> "actions" </> show actionId)
                                                   >>= return . fromResponse "action" . Right
                                           in (r, w))
                                   (authToken (ask w))

doShowDroplet  :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> Id -> (RESTT m (Result Droplet), w a)
doShowDroplet w dropletId = maybe (return $ error "no authentication token defined", w)
                            (\ t -> let r = fromResponse "droplet" . Right <$> getJSONWith (authorisation t) (toURI $ dropletsEndpoint </> show dropletId)
                                    in (r, w))
                            (authToken (ask w))

doSshInDroplet :: (MonadIO m) => w a -> Droplet -> (RESTT m (Result ()), w a)
doSshInDroplet w droplet =   let r = maybe (return $ error ("droplet " <> show droplet <> " has no public IP"))
                                     (\ip -> do
                                         s <- ssh ["root@" <> show ip ]
                                         case s of
                                          Left (e :: IOException)  -> return $ error (show e)
                                          Right () -> return $ Right ()
                                     )
                                     (publicIP droplet)
                             in (r, w)

waitForBoxToBeUp :: (Monad m) => Options -> Int -> Droplet -> RESTT m (Result Droplet)
waitForBoxToBeUp _    0 box  = return (Right box)
waitForBoxToBeUp opts n box  = do
  waitFor 1000000 ("waiting for droplet " ++ name box ++ " to become Active: " ++ show (n) ++ "s")
  b <- getJSONWith opts (toURI $ dropletsEndpoint </> show (dropletId box))
  case fromResponse "droplet" (Right b) of
   Right box'-> if status box' == Active
                then return (Right box')
                else waitForBoxToBeUp opts (n-1) box'
   err       -> return err

dropletCommandsInterpreter :: (MonadIO m, ComonadEnv ToolConfiguration w) => w a -> CoDropletCommands (RESTT m) (w a)
dropletCommandsInterpreter = CoDropletCommands
                             <$> queryList (Proxy :: Proxy Droplet)
                             <*> doCreate
                             <*> doDestroyDroplet
                             <*> doAction
                             <*> doGetAction
                             <*> doListSnapshots
                             <*> doSshInDroplet
                             <*> doShowDroplet

