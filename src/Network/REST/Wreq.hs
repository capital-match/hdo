{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation of @Net@ interface based on <http://wreq.io wreq>
module Network.REST.Wreq where

import           Control.Concurrent       (threadDelay)
import           Control.Lens             ((^.))
import           Control.Monad.Trans.Free
import           Data.Aeson               (Value, decode, eitherDecode)
import           Data.ByteString.Lazy     (ByteString)
import           Network.REST.Commands
import           Network.URI
import           Network.Wreq

-- | An implementation of @Net@ functor based on wreq and @IO@
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
        _ <- deleteWith opts (asString uri) :: IO (Response ByteString)
        runWreq k

      step (Free (Post uri val k)) = do
        resp <- post (asString uri) val :: IO (Response ByteString)
        let value :: Maybe Value = decode $ resp ^. responseBody
        runWreq (k value)

      step (Free (PostWith opts uri val k)) = do
        resp <- postWith opts (asString uri) val :: IO (Response ByteString)
        let value :: Either String Value = eitherDecode $ resp ^. responseBody
        runWreq (k value)


