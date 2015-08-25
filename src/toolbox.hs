{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Main where

import           Control.Exception      (catch, throw)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.IP                (IP)
import           Data.Maybe
import           Prelude                as P
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.IO.Error        (isDoesNotExistError)
import           Text.PrettyPrint

import           Commands
import           DigitalOcean           as DO
import           DO.Net
import           Names
import           Net
import           Pairing

generalOptions :: [OptDescr (ToolConfiguration -> ToolConfiguration)]
generalOptions = [ Option ['t'] ["auth-token"]
                   (ReqArg ( \ t config -> config { authToken = Just t}) "STRING")
                   "Authentication token used for communicating with server (default: <extracted from $AUTH_TOKEN environment)"
                 , Option ['q'] ["quiet"]
                   (NoArg ( \ config -> config { quiet = True}))
                   "Don't send notifications of operations to Slack (default: False)"
                 ]

createDropletOptions :: [OptDescr (BoxConfiguration -> BoxConfiguration)]
createDropletOptions = [ Option ['n'] ["name"]
                         (ReqArg ( \ n config -> config { configName = n }) "STRING")
                         "name of the box to create (default: <randomly generated name>)"
                       , Option ['r'] ["region"]
                         (ReqArg ( \ r config -> config { boxRegion = RegionSlug r}) "REGION")
                         "region where the instance is to be deployed (default: 'ams2')"
                       , Option ['s'] ["size"]
                         (ReqArg ( \ s config -> config { size = read s}) "SIZE")
                         "size of instance to deploy (default: '4gb')"
                       , Option ['i'] ["image-slug"]
                         (ReqArg ( \ i config -> config { configImageSlug = i}) "IMAGE")
                         "slug of image to deploy (default: 'ubuntu-14-04-x64')"
                       , Option ['k'] ["key"]
                         (ReqArg ( \ k config -> config { keys = read k ++ keys config}) "[KEY1,..]")
                         "add a key to access box (default: '[]')"
                       ]

getAuthFromEnv :: IO (Maybe AuthToken)
getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

getSlackUriFromEnv :: IO (Maybe URI)
getSlackUriFromEnv = (Just `fmap` getEnv "SLACK_URI") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)

defaultBox :: IO BoxConfiguration
defaultBox = do
  name <- generateName
  return $ BoxConfiguration name (RegionSlug "ams2") G4 defaultImage [429079]

defaultTool :: IO ToolConfiguration
defaultTool = do
  uri <- getSlackUriFromEnv
  tok <- getAuthFromEnv
  return $ Tool uri tok False

usage :: String
usage = usageInfo (banner ++ "\n" ++ usageInfo "General options:" generalOptions ++ "\nCommands options:") createDropletOptions
  where
    banner = "Usage: toolbox [OPTIONS..] COMMAND [CMD OPTIONS...]"

parseOptions  :: [String] -> IO (ToolConfiguration, [String])
parseOptions args = do
  d <- defaultTool
  case getOpt RequireOrder generalOptions args of
   (opts, coms, []) ->  return ((foldl (flip P.id) d opts), coms)
   (_,_,errs)       ->  ioError(userError (concat errs  ++ usage))


parseCommandOptions :: (MonadIO m) => [String] -> DOT m ()
parseCommandOptions ("droplets": "create":args) = do
  b <- liftIO defaultBox
  case getOpt Permute createDropletOptions args of
   (c,[],[])  -> createDroplet (foldl (flip P.id) b c) >>= outputResult
   (_,_,errs) -> liftIO $ ioError (userError (concat errs  ++ usage))
parseCommandOptions ("droplets": "destroy":dropletId:[]) = destroyDroplet (P.read dropletId)  >>= outputResult
parseCommandOptions ("droplets": "list":_)               = listDroplets  >>= outputResult
parseCommandOptions ("droplets": "power_off":dropletId:[])
                                                         = dropletAction (P.read dropletId) DoPowerOff >>= outputResult
parseCommandOptions ("droplets": "power_on":dropletId:[])
                                                         = dropletAction (P.read dropletId) DoPowerOn >>= outputResult
parseCommandOptions ("droplets": "action":dropletId:actionId:[])
                                                         = getAction (P.read dropletId)  (P.read actionId) >>= outputResult
parseCommandOptions ("images": "list":_)                 = listImages >>= outputResult
parseCommandOptions ("keys":"list":_)                    = listKeys >>= outputResult
parseCommandOptions ("sizes":"list":_)                   = listSizes >>= outputResult
parseCommandOptions e                                    = fail $ "I don't know how to interpret commands " ++ unwords e

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  (opts, cmds) <- parseOptions args
  runWreq $ pairEffectM (\ _ b -> return b) (mkDOClient opts) (parseCommandOptions cmds)

class (Show a) => Pretty a where
  pretty :: a -> Doc
  pretty = text . show

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty (Left a) = text "Error:" <+> pretty a
  pretty (Right a) = pretty a

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing  = text "-"

instance Pretty Char where
  pretty = char

instance Pretty Date where
  pretty (Date d) = text $ show d

instance Pretty Droplet where
  pretty Droplet{..} = integer id $$
                       (nest 5 $ (text name) <+> brackets (pretty status) <+> pretty region) $$
                       (nest 5 $ hcat $ punctuate (char '/') [pretty memory, pretty disk, int vcpus <+> text "cores"]) $$
                       (nest 5 $ pretty networks)

instance Pretty Status
instance Pretty NetType
instance Pretty IP

instance Pretty Region where
  pretty Region{..}     = text regionSlug
  pretty (RegionSlug s) = text s
  pretty NoRegion       = empty

instance Pretty Networks where
  pretty Networks{..} = text "IPv4" $$ nest 2 (pretty v4) $$
                        text "IPv6" $$ nest 2 (pretty v6)
  pretty NoNetworks   = text "N/A"

instance Pretty (Network a) where
  pretty NetworkV4{..} = pretty ip_address <> char '/' <> pretty netmask <+> brackets (pretty netType)
  pretty NetworkV6{..} = pretty ip_address <> char '/' <> int netmask_v6 <+> brackets (pretty netType)

instance Pretty (Bytes Giga) where
  pretty Bytes{..} = int bytesSize <> text "G"

instance Pretty (Bytes Mega) where
  pretty Bytes{..} = int bytesSize <> text "M"

instance (Pretty a) => Pretty [a] where
  pretty = sep . map pretty

instance Pretty Key where
  pretty Key{..} = integer keyId <+> text keyName <+> text keyFingerprint <+> text publicKey

instance Pretty Image where
  pretty Image{..} = integer imageId <+> text imageName <+> text distribution

instance Pretty TransferRate
instance Pretty SizeSlug

instance Pretty Size where
  pretty Size{..} = pretty szSlug $$
                    nest 5 (hcat $ punctuate (char '/') [pretty szMemory, int szVcpus, pretty szDisk, pretty szTransfer]) $$
                    nest 5 (pretty szPrice_Hourly <> text "$/h, " <> pretty szPrice_Monthly <> text "$/mo" ) $$
                    nest 5 (hcat $ punctuate (char ',') $ map pretty szRegions)

instance Pretty ActionResult where
  pretty ActionResult{..} = brackets (integer actionId) <+> pretty actionStartedAt <+> text "->" <+> pretty actionCompletedAt $$
                            (nest 5 $ integer actionResourceId <+> text (show actionType) <> char ':' <+> text (show actionStatus))

outputResult :: (Pretty a, MonadIO m) => a -> m  ()
outputResult = liftIO . putStrLn . render . pretty

