{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Network.DO.Pairing
import           Network.DO.Types
import           Prelude                      as P

-- functor for DO DSL
data DO a = ListDroplets ([Droplet] -> a)
          | CreateDroplet BoxConfiguration (Either String Droplet -> a)
          | DestroyDroplet Id (Maybe String -> a)
          | DropletAction Id Action (Either String ActionResult -> a)
          | GetAction Id Id (Either String ActionResult -> a)
          | ListKeys ([Key] -> a)
          | ListSizes ([Size] -> a)
          | ListImages ([Image] -> a)
          deriving (Functor)

-- free transformer to embed effects
type DOT = FreeT DO

-- smart constructors
listDroplets :: (Monad m) => DOT m [Droplet]
listDroplets = liftF $ ListDroplets P.id

createDroplet :: (Monad m) => BoxConfiguration -> DOT m (Either String Droplet)
createDroplet conf = liftF $ CreateDroplet conf P.id

destroyDroplet :: (Monad m) => Id -> DOT m (Maybe String)
destroyDroplet did = liftF $ DestroyDroplet did P.id

dropletAction :: (Monad m) => Id -> Action -> DOT m (Either String ActionResult)
dropletAction did action = liftF $ DropletAction did action P.id

getAction :: (Monad m) => Id -> Id -> DOT m (Either String ActionResult)
getAction did actId = liftF $ GetAction did actId P.id

listKeys :: (Monad m) => DOT m [Key]
listKeys = liftF $ ListKeys P.id

listSizes :: (Monad m) => DOT m [Size]
listSizes = liftF $ ListSizes P.id

listImages  :: (Monad m) => DOT m [Image]
listImages = liftF $ ListImages P.id

-- dual type, for creating interpreters
data CoDO m k = CoDO { listDropletsH   :: (m [Droplet], k)
                     , createDropletH  :: BoxConfiguration -> (m (Either String Droplet), k)
                     , destroyDropletH :: Id -> (m (Maybe String), k)
                     , actionDropletH  :: Id -> Action -> (m (Either String ActionResult), k)
                     , getActionH      :: Id -> Id -> (m (Either String ActionResult), k)
                     , listKeysH       :: (m [Key], k)
                     , listSizesH      :: (m [Size], k)
                     , listImagesH     :: (m [Image], k)
                     } deriving Functor

-- Cofree closure of CoDO functor
type CoDOT m = CofreeT (CoDO m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDO m) DO m where
  pairM f (CoDO list _ _ _ _ _ _ _)    (ListDroplets k)       = pairM f list k
  pairM f (CoDO _ create _ _ _ _ _ _)  (CreateDroplet conf k) = pairM f (create conf) k
  pairM f (CoDO _ _ destroy _ _ _ _ _) (DestroyDroplet i k)   = pairM f (destroy i) k
  pairM f (CoDO _ _ _ action _ _ _ _)  (DropletAction i a k)  = pairM f (action i a) k
  pairM f (CoDO _ _ _ _ getA _ _ _)    (GetAction i i' k)        = pairM f (getA i i') k
  pairM f (CoDO _ _ _ _  _ ks _ _)     (ListKeys k)           = pairM f ks k
  pairM f (CoDO _ _ _ _  _ _ szs _)    (ListSizes k)          = pairM f szs k
  pairM f (CoDO _ _ _ _ _ _ _ imgs)    (ListImages k)         = pairM f imgs k
