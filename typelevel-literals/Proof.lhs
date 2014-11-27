> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}
>
> import GHC.TypeLits
> import GHC.Exts 
> import Data.Proxy
> import Data.Constraint
> import System.Environment
> import Unsafe.Coerce
> import Data.Singletons

Say I have a Proxy p of some type-level natural number:

> p :: forall (n :: Nat). Proxy n
> p = Proxy

Imagine I get p from user input like this:

As you suggested we can introduce a proof carrying datatype

> data Proof n  (c :: Constraint) where Proof :: Proxy n -> Proof n c

And runtime converter that checks constraint at runtime:

> fromSome :: SomeNat -> Maybe (Proof n (n <= 255))
> fromSome (SomeNat p)
>    | natVal p <= 255 = Just (Proof (Proxy :: Proxy n))
>    | otherwise = Nothing

> test :: IO ()
> test = do
>     n <- readLn :: IO Integer
>
>     case someNatVal n of
>       Nothing -> error "Input is not a natural number!"
>       Just sn -> case fromSome sn of
>                    Just p -> return $ f2 p
>                    _ -> error "Input if larger than 255"
> 

> f2 :: (c ~ (n <= 255)) => Proof n c -> ()
> f2 _ = ()

Second apporach that will allow to carry one constraints along the other
and constraint existential types

> data Proof2 :: (Nat -> Constraint) -> * where
>   Proof2 :: c n => Proxy n -> Proof2 c

As it's not possible to have partially applied types/constraints/typefamilies
we have to introduce our own:

> type family LessThen255 n :: Constraint where
>     LessThen255 f = (f <= 255)

> example1 = Proof2 (Proxy :: Proxy 3) :: Proof2 LessThen255

Third approach

> fromSome2 :: SomeNat -> Maybe (Proof2 LessThen255)
> fromSome2 (SomeNat p')
>   | 0 <= p && p < 255 = undefined
>   | otherwise         = Nothing
>   where p = natVal p'

> class KnownNat n => FindEq (n::Nat) where
>   findEq :: (KnownNat p) => Proxy n -> Proxy p -> Maybe (Proof2 LessThen255)

> instance FindEq 0 where findEq _ _ = Nothing

> instance (LessThen255 n, KnownNat (n-1), KnownNat n, FindEq (n-1)) => FindEq n where
>   findEq n p = case sameNat n p of
>                  Just refl -> Just (Proof2 n)
>                  Nothing   -> undefined -- findEq (Proxy :: Proxy (n-1)) p
