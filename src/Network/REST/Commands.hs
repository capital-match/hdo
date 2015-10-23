{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Functor and low-level mechanism to interact with a server using "REST" API.
-- This module exposes @Net@ functor which can be implemented using various stack.
-- In particular, we provide @Network.REST.Wreq@ module as a wreq-based implementation
-- and a test-only implementation which is used to check correctness of higher-level
-- code, in particular in relation with JSON serialization.
module Network.REST.Commands where

import           Control.Monad.Trans.Free (FreeT (..), liftF)
import           Data.Aeson               (Value)
import           Network.URI              (URI)
import           Network.Wreq             (Options)

data Net a = Get URI (Value -> a)
           | Post URI Value (Maybe Value -> a)
           | WaitFor Int String a
           | GetWith Options URI (Value -> a)
           | PostWith Options URI Value (Either String Value -> a)
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

postJSONWith :: (Monad m) => Options ->  URI -> Value -> NetT m (Either String Value)
postJSONWith opts uri json = liftF $ PostWith opts uri json id

deleteJSONWith :: (Monad m) => Options -> URI -> NetT m ()
deleteJSONWith opts uri = liftF $ DeleteWith opts uri ()

waitFor :: (Monad m) => Int -> String -> NetT m ()
waitFor delay message = liftF $ WaitFor delay message ()

