{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Functor and low-level mechanism to interact with a server using Wreq and JSON values.
module Net where

import           Control.Concurrent       (threadDelay)
import           Control.Lens             ((^.))
import           Control.Monad.Trans.Free
import           Data.Aeson               (Value, decode)
import           Data.ByteString.Lazy     (ByteString)
import           Network.URI
import           Network.Wreq

data Net a = Get URI (Value -> a)
           | Post URI Value (Maybe Value -> a)
           | WaitFor Int String a
           | GetWith Options URI (Value -> a)
           | PostWith Options URI Value (Maybe Value -> a)
           | DeleteWith Options URI a
           deriving (Functor)

type NetT = FreeT Net

-- smart constructors
getJSON :: (Monad m) => URI -> NetT m Value
getJSON uri = liftF $ Get uri id

getJSONWith :: (Monad m) => Options -> URI -> NetT m Value
getJSONWith opts uri = liftF $ GetWith opts uri id

postJSON :: (Monad m) => URI -> Value -> NetT m (Maybe Value)
postJSON uri json = liftF $ Post uri json id

postJSONWith :: (Monad m) => Options ->  URI -> Value -> NetT m (Maybe Value)
postJSONWith opts uri json = liftF $ PostWith opts uri json id

deleteJSONWith :: (Monad m) => Options -> URI -> NetT m ()
deleteJSONWith opts uri = liftF $ DeleteWith opts uri ()

waitFor :: (Monad m) => Int -> String -> NetT m ()
waitFor delay message = liftF $ WaitFor delay message ()

--notifySlack :: String -> ToolConfiguration -> IO ()
--notifySlack msg Tool{..} = when (not quiet) $ maybe (return ()) (\ u -> notifyToSlack u "#platformdev" msg) slackUri

-- default implementation based on wreq and IO
runWreq :: NetT IO r -> IO r
runWreq r = do
  mr <- runFreeT r
  step mr
    where
      asString :: URI -> String
      asString = ($ "") . uriToString id

      step (Pure value)       = return value

      step (Free (WaitFor delay message k))  = do
        putStrLn message
        threadDelay delay
        runWreq k

      step (Free (Get uri k)) = do
        b <- asJSON =<< get (asString uri) :: IO (Response Value)
        let value = b ^. responseBody
        runWreq (k value)

      step (Free (GetWith opts uri k)) = do
        b <- asJSON =<< getWith opts (asString uri) :: IO (Response Value)
        let value = b ^. responseBody
        runWreq (k value)

      step (Free (DeleteWith opts uri k)) = do
        _ <- deleteWith opts (asString uri) :: IO (Response ())
        runWreq k

      step (Free (Post uri val k)) = do
        resp <- post (asString uri) val :: IO (Response ByteString)
        let value :: Maybe Value = decode $ resp ^. responseBody
        runWreq (k value)

      step (Free (PostWith opts uri val k)) = do
        resp <- postWith opts (asString uri) val :: IO (Response ByteString)
        let value :: Maybe Value = decode $ resp ^. responseBody
        runWreq (k value)


