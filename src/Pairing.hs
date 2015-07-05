{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
module Pairing (Pairing(..)
               , PairingM(..)
               , pairEffect
               , pairEffectM
               , pairEffect'
               ) where

import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, runFreeT)
import           Data.Functor.Identity        (Identity (..))

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

class (Functor f, Functor g, Monad m) => PairingM f g m where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

instance (Monad m) => PairingM ((,) (m a)) ((->) a) m where
  pairM p (ma, b) g = ma >>= \ a -> p b (g a)

pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs

pairEffect' :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect' p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> p (extract s) x
    Free gs -> pair (pairEffect' p) (unwrap s) gs

pairEffectM :: (PairingM f g m, Comonad w, Monad m)
           => (a -> b -> m r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffectM p s c = do
  ma <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> p ma x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs

