{-# LANGUAGE ScopedTypeVariables #-}
module Network.DO(
  -- * Types
  Command,
  module Network.DO.Types,
  -- * Generic Commands
  listKeys, listSizes, listRegions, listImages,
  -- * Droplets Commands
  listDroplets, createDroplet, showDroplet, destroyDroplet,
  dropletAction, dropletConsole, getAction, listDropletSnapshots,
  -- * Utilities
  runDOEnv, getAuthFromEnv, outputResult,
  generateName,
  module Network.DO.Droplets.Utils) where

import           Control.Exception            (catch, throw)
import           Control.Monad.Trans.Free
import           Data.Functor.Sum
import qualified Network.DO.Commands          as C
import qualified Network.DO.Droplets.Commands as C
import           Network.DO.Droplets.Utils
import           Network.DO.Names
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Pretty
import           Network.DO.Types
import           Network.REST
import           System.Environment           (getEnv)
import           System.IO.Error              (isDoesNotExistError)

type Command w a = FreeT (Sum C.DO C.DropletCommands) (RESTT w) a

listKeys :: (Monad w) => Command w [Key]
listKeys = injl C.listKeys

listSizes :: (Monad w) => Command w [Size]
listSizes = injl C.listSizes

listImages  :: (Monad w) => Command w  [Image]
listImages = injl C.listImages

listRegions :: (Monad w) => Command w [Region]
listRegions = injl C.listRegions

listDroplets :: (Monad w) => Command w [Droplet]
listDroplets = injr $ C.listDroplets

createDroplet :: (Monad w) => BoxConfiguration -> Command w (Either Error Droplet)
createDroplet = injr . C.createDroplet

showDroplet :: (Monad w) => Integer -> Command w (Either Error Droplet)
showDroplet = injr . C.showDroplet

destroyDroplet :: (Monad w) => Integer -> Command w (Maybe String)
destroyDroplet = injr . C.destroyDroplet

dropletAction :: (Monad w) => Id -> Action -> Command w (Result ActionResult)
dropletAction did = injr . C.dropletAction did

dropletConsole :: (Monad w) => Droplet -> Command w (Result ())
dropletConsole = injr . C.dropletConsole

getAction :: (Monad w) => Id -> Id -> Command w (Result ActionResult)
getAction  did = injr . C.getAction did

listDropletSnapshots :: (Monad w) => Id -> Command w [Image]
listDropletSnapshots = injr . C.listDropletSnapshots

-- | Run DO actions, extracting authentication token from environment variable `AUTH_TOKEN`
runDOEnv :: Command IO a -> IO a
runDOEnv actions = do
  token <- getAuthFromEnv
  runWreq $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool Nothing token False) actions

getAuthFromEnv :: IO (Maybe AuthToken)
getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)
