{-# LANGUAGE GADTs, MagicHash #-}
import Data.Dependent.Map as D
import Data.GADT.Compare

import Unsafe.Coerce

import GHC.Generics
import GHC.Prim
import GHC.Types

data Hint a where
  HintPort   :: Hint Int
  HintHost   :: Hint String
  HintTeapot :: Hint Int

dataToTag x = I# (dataToTag# x)

instance GEq Hint where
  a `geq` b
     | dataToTag a == dataToTag b = Just (unsafeCoerce Refl)
     | otherwise                  = Nothing

instance GCompare Hint where

test :: DMap Hint
test = D.insert HintTeapot 7 $ D.insert HintPort 99 $ D.insert HintPort 7 D.empty
