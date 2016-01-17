{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Functor and low-level mechanism to interact with a server using Wreq and JSON values.
module Network.REST(module Network.REST.Commands
                   ,module Network.REST.Wreq
                   ,ssh) where

import           Network.REST.Commands
import           Network.REST.Wreq
-- TODO temporary kludge
import           Network.SSH
