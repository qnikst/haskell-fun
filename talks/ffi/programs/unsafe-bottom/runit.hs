{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Concurrent
import Control.Monad
import Foreign.C
foreign import ccall   safe "<bottom.h> bottom"   safe_bottom :: CInt -> IO ()
foreign import ccall unsafe "<bottom.h> bottom" unsafe_bottom :: CInt -> IO ()

main :: IO ()
main = do
  lock <- newEmptyMVar
  forkIO $ safe_bottom 1
  yield
  print "Pass"
  forkIO $ do
    forever $ putStrLn "." >> threadDelay 1000000
    putMVar lock ()
  forkOn 2 $ void $ forkOS $ yield >> unsafe_bottom 1
  print "Pass"
  takeMVar lock
