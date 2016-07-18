module Main

isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

main : IO ()
main = putStrLn "hello world"
