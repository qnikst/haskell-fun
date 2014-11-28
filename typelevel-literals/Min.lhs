> {-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs, ConstraintKinds, TypeFamilies, UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances, ViewPatterns #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> import GHC.TypeLits
> import Data.Constraint
> import Data.Proxy
> import Unsafe.Coerce
> import Data.Reflection
> import Control.Monad

> data Proof2 :: (Nat -> Constraint) -> * where
>   Proof2 :: c n => Proxy n -> Proof2 c 
>
> type family LessThen255 n :: Constraint where
>     LessThen255 f = (f <= 255)

> one :: (LessThen255 ~ c, c n) => Proxy n -> Proof2 c
> one = Proof2

> data Proof2D :: (Nat -> *) -> * where
>   Proof2D :: KnownNat n => c n -> Proxy n -> Proof2D c

> instance Show (Proof2D c) where
>   show (Proof2D _ k) = show $ natVal k

> data LessThen255D (n::Nat) where LessThen255D :: (n <= 255) => LessThen255D n

> oned :: KnownNat n => LessThen255D n -> Proxy n -> Proof2D LessThen255D
> oned = Proof2D

> c2d :: LessThen255 n => Proxy n -> LessThen255D n
> c2d _ = LessThen255D

> c2dk :: KnownNat n => Proxy n -> (Proxy n -> Integer) -- Dict (KnownNat n)
> c2dk _ = natVal 


> guessProof :: (KnownNat n, n <= 255) => SomeNat -> Proxy n -> Maybe (Proof2D LessThen255D)
> guessProof (SomeNat p) n = case sameNat p n of
>     Just _  -> Just $ Proof2D {-(c2dk n)-} LessThen255D n
>     Nothing -> Nothing

> data N = Z | S N

> type family Guesses (n::Nat) :: [Nat] where
>    Guesses 0 = '[0]
>    Guesses n = n ': Guesses (n-1)


> g :: Proxy n -> Proxy (Guesses n)
> g _ = Proxy

> class GuessProof (n :: [Nat]) where
>   proof :: SomeNat -> Proxy n -> Maybe (Proof2D LessThen255D)


> instance GuessProof '[] where
>   proof _ _ = Nothing

> instance (KnownNat n, n <= 255, GuessProof ns) => GuessProof (n ': ns) where
>   proof s p = guessProof s (inner p) `mplus` proof s (next p)
>    where inner :: Proxy (n ': ns) -> Proxy (n::Nat)
>          inner _ = Proxy
>          next :: Proxy (n ': ns) -> Proxy (ns::[Nat])
>          next _ = Proxy

> main = return ()

> type family C2N (n::N) :: Nat where
>   C2N 'Z = 0
>   C2N ('S x) = 1 + C2N x
