{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
module Network.DO.Pairing (Pairing(..)
               , PairingM(..)
               , pairEffect
               , pairEffectM
               , pairEffect'
               , injr, injl
               ) where

import           Control.Comonad              (Comonad, extract)
import           Control.Comonad.Trans.Cofree (CofreeT, unwrap)
import           Control.Monad.Trans.Free     (FreeF (..), FreeT, liftF,
                                               runFreeT)
import           Data.Functor.Coproduct
import           Data.Functor.Identity        (Identity (..))
import           Data.Functor.Product

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

instance (Monad m, PairingM f h m, PairingM g k m) => PairingM (Coproduct f g) (Product h k) m where
  pairM p (Coproduct (Left f)) (Pair h _)  = pairM p f h
  pairM p (Coproduct (Right g)) (Pair _ k) = pairM p g k

instance (Monad m, PairingM h f m, PairingM k g m) => PairingM (Product h k) (Coproduct f g) m where
  pairM p (Pair h _) (Coproduct (Left f))  = pairM p h f
  pairM p (Pair _ k) (Coproduct (Right g)) = pairM p k g

injl :: (Monad m, Functor f, Functor g) => f a -> FreeT (Coproduct f g) m a
injl = liftF . Coproduct . Left

injr :: (Monad m, Functor f, Functor g) => g a -> FreeT (Coproduct f g) m a
injr = liftF . Coproduct . Right

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

