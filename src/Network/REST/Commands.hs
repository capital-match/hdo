{-# LANGUAGE DeriveFunctor, OverloadedStrings, ScopedTypeVariables #-}
-- | Functor and low-level mechanism to interact with a server using "REST" API.
--
-- This module exposes @REST@ functor which can be implemented using various stack.
-- In particular, we provide @Network.REST.Wreq@ module as a wreq-based implementation
-- and a test-only implementation which is used to check correctness of higher-level
-- code, in particular in relation with JSON serialization.
module Network.REST.Commands where

import           Control.Monad.Trans.Free (FreeT (..), liftF)
import           Data.Aeson               (Value)
import           Data.Monoid
import           Data.Text                (Text, pack)
import           Network.URI              (URI)

data Options = Header Text [ Text ]

authorisation :: String -> Options
authorisation t = Header "Authorization" [ "Bearer " <> pack t]

data REST a = Get URI (Value -> a)
           | Post URI Value (Maybe Value -> a)
           | WaitFor Int String a
           | GetWith Options URI (Value -> a)
           | PostWith Options URI Value (Either String Value -> a)
           | DeleteWith Options URI a
           deriving (Functor)

type RESTT = FreeT REST

-- smart constructors
getJSON :: (Monad m) => URI -> RESTT m Value
getJSON uri = liftF $ Get uri id

getJSONWith :: (Monad m) => Options -> URI -> RESTT m Value
getJSONWith opts uri = liftF $ GetWith opts uri id

postJSON :: (Monad m) => URI -> Value -> RESTT m (Maybe Value)
postJSON uri json = liftF $ Post uri json id

postJSONWith :: (Monad m) => Options ->  URI -> Value -> RESTT m (Either String Value)
postJSONWith opts uri json = liftF $ PostWith opts uri json id

deleteJSONWith :: (Monad m) => Options -> URI -> RESTT m ()
deleteJSONWith opts uri = liftF $ DeleteWith opts uri ()

waitFor :: (Monad m) => Int -> String -> RESTT m ()
waitFor delay message = liftF $ WaitFor delay message ()

