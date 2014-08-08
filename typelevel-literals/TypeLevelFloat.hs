{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module TypeLevelFloat
  ( mkFloatProxy
  , mkProxy
  ) where

import Data.Ratio
import Data.Proxy
import Language.Haskell.TH
import GHC.Real
import TypeLevelRatio
import Language.Haskell.TH.Syntax
import GHC.TypeLits

mkFloatProxy :: RealFrac a => a -> Q Exp
mkFloatProxy x = [| Proxy :: Proxy ($(nk a) :%% $(nk b)) |]
  where (a :% b) = toRational x
        nk x = sigT (litT (numTyLit x)) (ConT $ mkName "Nat")

mkProxy :: Q Exp
mkProxy = [| Proxy :: Proxy ($(sigT (litT (numTyLit i)) (ConT $ mkName "Nat"))) |]
  where  i = 3
