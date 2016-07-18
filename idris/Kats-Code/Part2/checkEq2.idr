checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat (S Z) Z = Nothing
checkEqNat Z (S Z) = Nothing
checkEqNat (S a) (S b) = case checkEqNat a b of
  Nothing => Nothing
  Just Refl => Just Refl
