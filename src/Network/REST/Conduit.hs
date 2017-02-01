{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation of @REST@ interface based on <https://haskell-lang.org/library/http-client http-conduit>
-- standard library
module Network.REST.Conduit where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.Trans.Free
import           Data.Aeson               (Value, decode, eitherDecode)
import           Data.CaseInsensitive     (mk)
import           Data.Functor             (void)
import           Data.Text.Encoding       (encodeUtf8)
import           Network.HTTP.Simple
import           Network.REST.Commands
import           Network.URI

-- | An implementation of @REST@ functor based on http-client and @IO@
runConduit :: RESTT IO r -> IO r
runConduit r = do
  mr <- runFreeT r
  step mr
    where
      asString :: URI -> String
      asString = ($ "") . uriToString id

      step (Pure value)       = return value

      step (Free (WaitFor delay message k))  = do
        putStrLn message
        threadDelay delay
        runConduit k

      step (Free (Get uri k)) = do
        request <- parseRequest $ "GET " ++ asString uri
        response <- httpJSON request
        let value = getResponseBody response :: Value
        runConduit (k value)

      step (Free (GetWith opts uri k)) = do
        request <- parseRequest $ "GET " ++ asString uri
        response <- httpJSON $ options opts request
        let value = getResponseBody response :: Value
        runConduit (k value)

      step (Free (DeleteWith opts uri k)) = do
        request <- parseRequest $ "DELETE " ++ asString uri
        void $ httpLBS $ options opts request
        runConduit k

      step (Free (Post uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        response <- httpLbs $ setRequestBodyJSON val request
        let value = decode $ getResponseBody response
        runConduit (k value)

      step (Free (PostWith opts uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        response <- httpLbs $ options opts $ setRequestBodyJSON val request
        let value :: Either String Value = eitherDecode $ getResponseBody response
        runConduit (k value)


options :: Options -> Request -> Request
options (Header h c) = setRequestHeader (mk $ encodeUtf8 h) (map encodeUtf8 c)
