data Tree a = Empty
            | Node (Tree a) a (Tree a)

insert : Ord a => a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node l b r) =
   if a <= b
   then Node (insert a l) b r
   else Node l b (insert a r)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Value Int

evaluate : Expr -> Int
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2
evaluate (Value i)   = i



data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol


wheels : Vehicle power -> Nat
wheels Bycycle = 2
wheels (Car _) = 4
wheels (Bus _) = 6


refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Bus _) = Bus 100
