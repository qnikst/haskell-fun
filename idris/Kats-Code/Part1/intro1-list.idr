{- Definition from the Prelude:

infixr 5 ::

data List a = Nil | (::) a (List a)
-}

my_append : List a -> List a -> List a
my_append Nil a = a
my_append (a :: as) bs = a :: my_append as bs

my_map : (a -> b) -> List a -> List b
my_map f Nil = Nil
my_map f (a :: as) = f a :: my_map f as

my_zipWith : (a -> b -> c) -> List a -> List b -> List c
my_zipWith f (a :: as) (b :: bs) = f a b :: my_zipWith f as bs
my_zipWith _ _ _ = Nil
