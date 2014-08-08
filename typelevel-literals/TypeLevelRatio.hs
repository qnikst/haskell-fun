{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module TypeLevelRatio
  ( (:%%)
  , numerator
  , denomenator
  ) where

import Data.Proxy
import GHC.TypeLits

data (:%%) a b

-- | extract numerator type
numerator :: proxy (n :%% m) -> Proxy n
numerator _ = Proxy

-- | Extract denomenator type
denomenator :: proxy (n :%% m)  -> Proxy m
denomenator _ = Proxy
