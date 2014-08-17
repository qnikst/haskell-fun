{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy

g1 :: Integer -> Double
g1 p =  1 / (2 - 2**(1/(fromIntegral p+1)))

g2 :: Integer -> Double
g2 p = - 2**(1/(fromIntegral p+1)) / ( 2 - 2**(1/(fromIntegral p+1)))


g3 :: Integer -> Double
g3 = g1
-- [g1, g2, g1]

-- Apply a method
t :: Integer -> Double -> [Double]
t p dt = map (*dt) [g1 p, g2 p, g3 p]

-- Upgrade a method
ut :: Integer -> [Double] -> [Double]
ut p xs = xs >>= (\x -> t (p+2) x)


data RK2 = RK2

type family Order a :: Nat
type instance Order RK2 = 2

type family IsSymmetric a :: Constraint
type instance IsSymmetric RK2 = ()

buildComposePoints :: forall p . KnownNat (Order p)
                   => p -> Double -> [Double]
buildComposePoints p dt = map (*dt) [g1 o, g2 o, g3 o]
  where
    o = natVal (Proxy :: Proxy (Order p))

buildComposePointsSym :: forall p n . (UpdateCompose (Order p + 2) n, IsSymmetric p, KnownNat (Order p), KnownNat n)
                      => p -> Proxy n -> Double -> [Double]
buildComposePointsSym p pn dt = update (Proxy :: Proxy ((Order p) + 2)) pn (buildComposePoints p dt)

class UpdateCompose (k :: Nat) (v::Nat) where
  update :: Proxy k -> Proxy v -> [Double] -> [Double]

class UpdateComposeCase (leq :: Bool) (k :: Nat) (v :: Nat) where
  updateCase :: Proxy leq -> Proxy k -> Proxy v -> [Double] -> [Double]

instance UpdateComposeCase (k <=? v) k v => UpdateCompose k v where
  update = updateCase (Proxy :: Proxy (k <=? v))

instance UpdateComposeCase False k v where
  updateCase _ _ _ = id
  
instance (KnownNat k, UpdateCompose (k+2) v) => UpdateComposeCase True k v where
  updateCase _ k v ds = update (plus2 k) v (ds >>= \x -> map (*x) [g1 o, g2 o, g3 o])
    where
      o = natVal k
      plus2 :: Proxy n -> Proxy (n+2)
      plus2 _ = Proxy
