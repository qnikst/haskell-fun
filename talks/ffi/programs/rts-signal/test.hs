{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign.C.Error
import Control.Concurrent
import Control.Monad
import Data.Time

foreign import ccall safe bogus :: IO CSize

hsBogus :: IO Int
hsBogus = fmap fromIntegral bogus

main = do
  forkIO $ do yield
              print =<< getCurrentTime
              ret <- hsBogus
              print =<< getCurrentTime
              print ret
              en <- getErrno
              print (errnoToIOError "none" en Nothing Nothing)
  forever $ do putStrLn "."
               threadDelay 1000000
