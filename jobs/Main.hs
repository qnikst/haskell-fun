import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.Foldable

newtype R a = R (TBQueue (TMVar a))

steal :: TBQueue (IO b) -> R b -> STM (IO b, TMVar b)
steal q (R r) = do 
  x <- readTBQueue q
  out <- newEmptyTMVar
  writeTBQueue r out
  pure (x, out)

await :: R a -> STM a
await (R r) = do
  m <- readTBQueue r
  takeTMVar m
  

main = do
  input <- newTBQueueIO 10
  output <- R <$> newTBQueueIO 10
  replicateM 5 $ forkIO $ forever $ do
    (io, b) <- atomically $ steal input output
    atomically . putTMVar b =<< io
  for_ tasks $ atomically . writeTBQueue input 
  forever $ do
    print =<< atomically (await output)
  where
    tasks = zipWith go  [(0::Int)..]
      [ 300000
      , 200000
      , 150000
      , 600000
      , 140000
      ]
    go n t = do
      putStrLn $ show n <> ">"
      threadDelay t
      putStrLn $ "<" <> show n
      pure n

