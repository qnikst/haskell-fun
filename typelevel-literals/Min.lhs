> {-# LANGUAGE DataKinds, TypeOperators, KindSignatures, GADTs, ConstraintKinds, TypeFamilies, UndecidableInstances #-}
> 
> import GHC.TypeLits
> import Data.Constraint
> import Data.Proxy

> data Proof2 :: (Nat -> Constraint) -> Nat -> * where
>   Proof2 :: c n => Proxy n -> Proof2 c n
>
> type family LessThen255 n :: Constraint where
>     LessThen255 f = (f <= 255)
>
> one :: LessThen255 n => Proxy n -> Proof2 LessThen255 n
> one = Proof2
