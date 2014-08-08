{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Ratio (Ratio(..))
import Data.Proxy
import GHC.Real (Ratio(..))
import GHC.TypeLits
import Language.Haskell.TH

import TypeLevelRatio
import TypeLevelFloat

import Data.Tagged

toProxy :: Tagged n a  -> Proxy n
toProxy _ = Proxy

mkT :: proxy n -> a -> Tagged n a
mkT _ a = Tagged a

useT :: (KnownNat n, Num a) => Tagged n a -> a
useT t@(Tagged a) = fromIntegral (natVal (toProxy t)) + a

useTF :: (KnownNat n, KnownNat m, Fractional a) => Tagged (n :%% m) a -> a
useTF t@(Tagged a) = v + a
  where v = fromRational $
             natVal (numerator $ toProxy t) :% natVal (denomenator $ toProxy t)

test = mkT $(mkFloatProxy pi) 7.0

{-
-- *Main> mkT (Proxy::Proxy 3) 1 + mkT (Proxy::Proxy 3) 2
-- Loading package tagged-0.7.2 ... linking ... done.
-- Tagged 3
-}

(^+) :: (KnownNat n, KnownNat m) => Tagged n a -> Tagged m a -> Maybe (Tagged n a)
a (^+) b =
  case (sameNat `on` toProxy) a b
    Just x -> Just (retag 
