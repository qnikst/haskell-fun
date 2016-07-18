data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a

%name Vect xs, ys, zs

data EqNat : Nat -> Nat -> Type where
     Same : (x : Nat) -> EqNat x x

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS p p (Same p) = Same (S p)


checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat (S t) Z = Nothing
checkEqNat Z (S t) = Nothing
checkEqNat (S a) (S b) = case checkEqNat a b of
  Nothing => Nothing
  Just (Same p) => Just (Same (S p))


exactLength : {m:Nat} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m=m} l v = case checkEqNat l m of
  Nothing => Nothing
  Just (Same p) => Just v
