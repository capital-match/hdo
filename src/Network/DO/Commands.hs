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
data DO a = ListKeys ([Key] -> a)
          | ListSizes ([Size] -> a)
          | ListImages ([Image] -> a)
          | ListRegions ([Region] -> a)
          | ListFloatingIPs ([FloatingIP] -> a)
          deriving (Functor)

-- free transformer to embed effects
type DOT = FreeT DO

-- smart constructors
listKeys :: DO [Key]
listKeys = ListKeys P.id

listSizes :: DO [Size]
listSizes = ListSizes P.id

listImages  :: DO [Image]
listImages = ListImages P.id

listRegions :: DO [Region]
listRegions = ListRegions P.id

listFloatingIPs :: DO [FloatingIP]
listFloatingIPs = ListFloatingIPs P.id

-- dual type, for creating interpreters
data CoDO m k = CoDO { listKeysH        :: (m [Key], k)
                     , listSizesH       :: (m [Size], k)
                     , listImagesH      :: (m [Image], k)
                     , listRegionsH     :: (m [Region], k)
                     , listFloatingIPsH :: (m [FloatingIP], k)
                     } deriving Functor

-- Cofree closure of CoDO functor
type CoDOT m = CofreeT (CoDO m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDO m) DO m where
  pairM f (CoDO ks _  _ _ _)  (ListKeys k)   = pairM f ks k
  pairM f (CoDO _ szs _ _ _)  (ListSizes k)  = pairM f szs k
  pairM f (CoDO _ _ imgs _ _)  (ListImages k) = pairM f imgs k
  pairM f (CoDO _ _ _ rgns _)  (ListRegions k) = pairM f rgns k
  pairM f (CoDO _ _ _ _ flips)  (ListFloatingIPs k) = pairM f flips k
