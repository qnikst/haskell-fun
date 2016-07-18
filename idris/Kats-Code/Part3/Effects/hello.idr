import Effects
import Effect.StdIO
import Effect.Exception

hello : Eff () [STDIO]
hello = do putStr "What is your name? "
           x <- getStr
           putStrLn ("Hello " ++ x)

main : IO ()
main = run hello
