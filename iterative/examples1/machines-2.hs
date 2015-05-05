{-# LANGUAGE Rank2Types #-}
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Machine
import System.IO

import Control.Monad ( unless )
import System.Environment ( getArgs )
import Control.Monad.Managed

lineSource :: FilePath -> SourceT (ResourceT IO) String
lineSource fp = construct $ do
    hdl <- fmap snd $ lift $ allocate (openFile fp ReadMode) hClose
    go hdl
  where
    go h = do c <- liftIO $ hIsEOF h
              unless c $ do
                l <- liftIO $ hGetLine h
                yield l
                go h

andThen :: Monad m => MachineT m (k a) b -> MachineT m (k a) b -> MachineT m (k a) b
andThen s1 s2 = MachineT $ do
    step <- runMachineT s1
    case step of
      Stop         -> runMachineT s2
      Yield o r    -> return $ Yield o (andThen r s2)
      Await f kt r -> return $ Await (flip andThen s2 . f) kt (andThen r s2)

numLines :: Monad m => MachineT m (Is a) Int
numLines = fold step (0::Int) where
  step s _ = (s+1)


main :: IO ()
main = do
  (f1:f2: _) <- getArgs
  print . head =<< runResourceT (runT $ (lineSource f1 `andThen` lineSource f2) ~> numLines)
