{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Domain.Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Data.IP
import           Network.DO.Pairing
import           Network.DO.Types
import           Prelude                      as P

data DomainCommands a = ListDomains ([Domain] -> a)
                      | CreateDomain DomainName IP (Result Domain -> a)
                      | DeleteDomain DomainName (Maybe String -> a)
                      deriving (Functor)

type DomainCommandsT = FreeT DomainCommands

-- smart constructors
listDomains :: DomainCommands [Domain]
listDomains = ListDomains P.id

createDomain :: DomainName -> IP -> DomainCommands (Result Domain)
createDomain name ip = CreateDomain name ip P.id

deleteDomain :: DomainName -> DomainCommands (Maybe String)
deleteDomain ip = DeleteDomain ip P.id

-- dual type, for creating interpreters
data CoDomainCommands m k =
  CoDomainCommands { listDomainsH  :: (m [Domain], k)
                   , createDomainH :: DomainName -> IP -> (m (Result Domain), k)
                   , deleteDomainH :: DomainName -> (m (Maybe String), k)
                   } deriving Functor

-- Cofree closure of CoDomainCommands functor
type CoDomainCommandsT m = CofreeT (CoDomainCommands m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDomainCommands m) DomainCommands m where
  pairM f (CoDomainCommands ks _ _ )    (ListDomains k)      = pairM f ks k
  pairM f (CoDomainCommands _  tgt _ )  (CreateDomain n i k) = pairM f (tgt n i) k
  pairM f (CoDomainCommands _  _  del ) (DeleteDomain n k)   = pairM f (del n) k
