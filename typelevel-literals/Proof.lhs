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
>
> import GHC.TypeLits
> import GHC.Exts 
> import Data.Proxy
> import System.Environment

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

> main :: IO ()
> main = do
>     [arg] <- getArgs
>     let n = read arg :: Integer
>
>     case someNatVal n of
>       Nothing -> error "Input is not a natural number!"
>       Just sn -> case fromSome sn of
>                    Just p -> return $ f2 p
>                    _ -> error "Input if larger than 255"
> 

> f2 :: (c ~ (n <= 255)) => Proof n c -> ()
> f2 _ = ()

