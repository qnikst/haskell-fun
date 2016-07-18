{-
data Nat = Z | S Nat

(+) : Nat -> Nat -> Nat
Z + y = y
(S x) + y = S (plus x y)
-}

data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs


append : Vect n a -> Vect m a -> Vect (n + m) a
append Nil v = v
append (a :: as) v = a :: append as v


zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
zipWith f (a :: as) (b :: bs) = f a b :: zipWith f as bs
zipWith _ Nil Nil = Nil

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip as bs = Main.zipWith (\a, b => (a,b)) as bs

mkvec : {m:Nat} -> Vect m (Vect Z elem)
mkvec {m=Z}   = Nil
mkvec {m=S k} = Nil :: mkvec

transpose_vec : Vect n (Vect m elem) -> Vect m (Vect n elem)
transpose_vec Nil = mkvec
transpose_vec (x :: xs) = zipWith (::) x (transpose_vec xs)


n0 : Vect 0 Nat
n0 = Nil

n1 : Vect 1 Nat
n1 = 1 :: Nil





------- A main program to read dimensions, generate and tranpose a vector

Functor (Vect m) where
    map m [] = []
    map m (x :: xs) = m x :: map m xs

Show a => Show (Vect m a) where
    show x = show (toList x)
      where
        toList : Vect m a -> List a
        toList [] = []
        toList (y :: xs) = y :: toList xs

countTo : (m : Nat) -> Vect m Int
countTo Z = []
countTo (S k) = 0 :: map (+1) (countTo k)

mkVect : (n, m : Nat) -> Vect n (Vect m Int)
mkVect Z m = []
mkVect (S k) m = countTo m :: map (map (+ cast m)) (mkVect k m)

main : IO ()
main = do putStr "Rows: "
          let r : Nat = cast (cast {to=Int} !getLine)
          putStr "Columns: "
          let c : Nat = cast (cast {to=Int} !getLine)
          printLn (mkVect r c)
          putStrLn "Transposed:"
          printLn (transpose_vec (mkVect r c))
