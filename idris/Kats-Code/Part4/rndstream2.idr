import Data.Primitives.Views

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `div` 64) :: randoms seed'

data Face = Heads | Tails

Eq Face where
    (==) Heads Heads = True
    (==) Tails Tails = True
    (==) _ _ = False

total
getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * div) + rem) | (DivBy prf) 
       = case rem of
              0 => Heads
              _ => Tails
  
coinFlips : Nat -> Stream Int -> List Face
coinFlips k rnds = map getFace (take k rnds)

