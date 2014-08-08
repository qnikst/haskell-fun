module Main where

import System.INotify
import System.Environment
import System.Process
import System.IO
import Control.Concurrent
import Control.Monad

main =
    withINotify $ \inot -> do
      (f:_) <- getArgs
      x <- addWatch inot [Modify] f (const $ go inot f)
      hClose =<< openFile "iot.handle" WriteMode 
      e <- newEmptyMVar
      addWatch inot [Delete] "iot.handle" (const $ putMVar e ())
      takeMVar e
  where
    go inot f = do
      putStrLn "on"
      void . waitForProcess =<< runCommand "make"
      addWatch inot [Modify] f (const $ go inot f)
      putStrLn "off"
  
