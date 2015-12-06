import Data.Foldable (forM_)
import Control.Concurrent
import System.Environment
-- Don't do this, only for demo purposes:
import Control.Exception (evaluate)
import Control.DeepSeq (force)

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  [i,l] <- map read <$> getArgs
  lock <- newQSem i
  for [0..i] $ \z -> do
    waitQSem lock
    forkOn z $ do
      forM_ [0..l] $ evaluate . force . (!!) fibs
      signalQSem lock
  replicateM_ 3 $ waitQSem lock
