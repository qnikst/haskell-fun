module l1

data UL : Nat -> UniqueType -> UniqueType where
  Nil  : {a:UniqueType} -> UL Z a
  (::) : {a:UniqueType} -> a -> UL k a -> UL (S k) a


data UNat : UniqueType where
  UZ : UNat
  US : UNat -> UNat

usort : UL n UNat -> UL n UNat
usort Nil = Nil
usort (x :: xs) = insert x (usort xs) where
  mutual
    go : Borrowed UNat -> Borrowed UNat -> UNat -> UNat -> UL n UNat -> UL (S (S n)) UNat
    go UZ _ a b bs = a :: b :: bs
    go _ UZ a b bs = b :: (insert a bs)
    go (US n) (US k) a b bs = go n k a b bs
    insert : UNat -> UL n UNat -> UL (S n) UNat
    insert a Nil = a :: Nil
    insert a (b :: bs) = go (lend a) (lend b) a b bs
