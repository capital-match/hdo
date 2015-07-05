{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           DigitalOcean
import           Prelude                      as P

import           Pairing

-- functor for DO DSL
data DO a = ListDroplets ([Droplet] -> a)
          | CreateDroplet BoxConfiguration (Either String Droplet -> a)
          | DestroyDroplet Id (Maybe String -> a)
          | ListKeys ([Key] -> a)
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
destroyDroplet dropletId = liftF $ DestroyDroplet dropletId P.id

listKeys :: (Monad m) => DOT m [Key]
listKeys = liftF $ ListKeys P.id

listImages  :: (Monad m) => DOT m [Image]
listImages = liftF $ ListImages P.id

-- dual type, for creating interpreters
data CoDO m k = CoDO { listDropletsH   :: (m [Droplet], k)
                     , createDropletH  :: BoxConfiguration -> (m (Either String Droplet), k)
                     , destroyDropletH :: Id -> (m (Maybe String), k)
                     , listKeysH       :: (m [Key], k)
                     , listImagesH     :: (m [Image], k)
                     } deriving Functor

-- Cofree closure of CoDO functor
type CoDOT m = CofreeT (CoDO m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDO m) DO m where
  pairM f (CoDO list _ _ _ _)    (ListDroplets k)       = pairM f list k
  pairM f (CoDO _ create _ _ _)  (CreateDroplet conf k) = pairM f (create conf) k
  pairM f (CoDO _ _ destroy _ _) (DestroyDroplet i k)   = pairM f (destroy i) k
  pairM f (CoDO _ _ _ ks _)      (ListKeys k)           = pairM f ks k
  pairM f (CoDO _ _ _ _ imgs)    (ListImages k)         = pairM f imgs k
