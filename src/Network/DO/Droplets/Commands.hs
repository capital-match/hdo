{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Droplets.Commands(DropletCommands,
                                    DropletCommandsT,CoDropletCommandsT,
                                    CoDropletCommands(CoDropletCommands),
                                    listDroplets, createDroplet, destroyDroplet, dropletAction,
                                    showDroplet, getAction, listDropletSnapshots) where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Network.DO.Pairing
import           Network.DO.Types
import           Prelude                      as P

-- | Available commands for droplets
data DropletCommands a = ListDroplets ([Droplet] -> a)
                       | CreateDroplet BoxConfiguration (Either String Droplet -> a)
                       | DestroyDroplet Id (Maybe String -> a)
                       | DropletAction Id Action (Either String ActionResult -> a)
                       | GetAction Id Id (Either String ActionResult -> a)
                       | ListSnapshots Id ([Image] -> a)
                       | ShowDroplet Id (Either String Droplet -> a)
                       deriving (Functor)

-- free transformer to embed effects
type DropletCommandsT = FreeT DropletCommands

-- smart constructors
listDroplets :: DropletCommands [Droplet]
listDroplets = ListDroplets P.id

createDroplet :: BoxConfiguration -> DropletCommands (Either String Droplet)
createDroplet conf = CreateDroplet conf P.id

showDroplet :: Id -> DropletCommands (Either String Droplet)
showDroplet did = ShowDroplet did P.id

destroyDroplet :: Id -> DropletCommands (Maybe String)
destroyDroplet did = DestroyDroplet did P.id

dropletAction :: Id -> Action -> DropletCommands (Either String ActionResult)
dropletAction did action = DropletAction did action P.id

getAction :: Id -> Id -> DropletCommands (Either String ActionResult)
getAction did actId = GetAction did actId P.id

listDropletSnapshots :: Id -> DropletCommands [Image]
listDropletSnapshots did = ListSnapshots did P.id


-- | Comonadic interpreter for @DropletCommands@
data CoDropletCommands m k = CoDropletCommands { listDropletsH   :: (m [Droplet], k)
                                               , createDropletH  :: BoxConfiguration -> (m (Either String Droplet), k)
                                               , destroyDropletH :: Id -> (m (Maybe String), k)
                                               , actionDropletH  :: Id -> Action -> (m (Either String ActionResult), k)
                                               , getActionH      :: Id -> Id -> (m (Either String ActionResult), k)
                                               , listSnapshotsH  :: Id -> (m [Image], k)
                                               , showDropletH    :: Id -> (m (Either String Droplet), k)
                                               } deriving Functor

-- Cofree closure of CoDO functor
type CoDropletCommandsT m = CofreeT (CoDropletCommands m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDropletCommands m) DropletCommands m where
  pairM f (CoDropletCommands list _ _ _ _ _ _)       (ListDroplets k)       = pairM f list k
  pairM f (CoDropletCommands _ create _ _ _ _ _)     (CreateDroplet conf k) = pairM f (create conf) k
  pairM f (CoDropletCommands _ _ destroy _ _ _ _)    (DestroyDroplet i k)   = pairM f (destroy i) k
  pairM f (CoDropletCommands _ _ _ action _ _ _)     (DropletAction i a k)  = pairM f (action i a) k
  pairM f (CoDropletCommands _ _ _ _ getA _ _)       (GetAction i i' k)     = pairM f (getA i i') k
  pairM f (CoDropletCommands _ _ _ _ _  snapshots _) (ListSnapshots i k)    = pairM f (snapshots i) k
  pairM f (CoDropletCommands _ _ _ _ _  _ showD)     (ShowDroplet i k)      = pairM f (showD i) k
