{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.IP.Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Data.IP
import           Network.DO.Pairing
import           Network.DO.Types
import           Prelude                      as P

-- functor for DO DSL
data IPCommands a = ListFloatingIPs ([FloatingIP] -> a)
                  | CreateIP FloatingIPTarget (Result FloatingIP -> a)
                  | DeleteIP IP (Maybe String -> a)
          deriving (Functor)

-- free transformer to embed effects
type IPCommandsT = FreeT IPCommands

-- smart constructors
listFloatingIPs :: IPCommands [FloatingIP]
listFloatingIPs = ListFloatingIPs P.id

createFloatingIP :: FloatingIPTarget -> IPCommands (Result FloatingIP)
createFloatingIP target = CreateIP target P.id

deleteFloatingIP :: IP -> IPCommands (Maybe String)
deleteFloatingIP ip = DeleteIP ip P.id

-- dual type, for creating interpreters
data CoIPCommands m k =
  CoIPCommands { listFloatingIPsH  :: (m [FloatingIP], k)
               , createFloatingIPH :: FloatingIPTarget -> (m (Result FloatingIP), k)
               , deleteIPH         :: IP -> (m (Maybe String), k)
               } deriving Functor

-- Cofree closure of CoIPCommands functor
type CoIPCommandsT m = CofreeT (CoIPCommands m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoIPCommands m) IPCommands m where
  pairM f (CoIPCommands ks _ _)    (ListFloatingIPs k)   = pairM f ks k
  pairM f (CoIPCommands _  tgt _)  (CreateIP i k)        = pairM f (tgt i) k
  pairM f (CoIPCommands _  _  del) (DeleteIP i k)        = pairM f (del i) k
