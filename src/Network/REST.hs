{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
-- | Functor and low-level mechanism to interact with a server using Wreq and JSON values.
module Network.REST(module M
                   ,ssh) where

import           Network.REST.Commands as M
import           Network.REST.Conduit  as M
-- TODO temporary kludge
import           Network.SSH
