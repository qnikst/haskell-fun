%default total

data InfIO : Type -> Type where
     Do : IO a -> (a -> Inf (InfIO b)) -> InfIO b

echo_name : InfIO ()
echo_name = Do (putStr "Enter your name: ") $ \_ =>
            Do getLine $ \name =>
            Do (putStrLn ("Hello " ++ name)) $ \_ =>
            echo_name

data Fuel = Dry | More (Lazy Fuel)

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO a -> IO (Maybe a)
run (More fuel) (Do c f) = do res <- c
                              run fuel (f res)
run Dry p = pure Nothing

partial
forever : Fuel
forever = More forever

partial
main : IO ()
main = do run (tank 10) echo_name
          pure ()
