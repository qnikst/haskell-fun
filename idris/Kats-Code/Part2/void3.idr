twoplustwo_not_five : 2 + 2 = 5 -> Void
twoplustwo_not_five = twoplustwo_not_five


value_not_suc : (x : Nat) -> x = S x -> Void
value_not_suc x Relf = value_not_suc (S x) Relf



-- The following definitions are not total, so can't be accepted as proofs

loop : Void
loop = loop

nohead : Void
nohead = getHead []
  where
    getHead : List Void -> Void
    getHead (x :: xs) = x
