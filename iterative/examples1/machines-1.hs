{-# LANGUAGE Rank2Types #-}
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Machine
import System.IO

import Control.Monad ( unless )
import System.Environment ( getArgs )

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

numLines :: Monad m => MachineT m (Is a) Int
numLines = fold step (0::Int) where
  step s _ = (s+1)

main :: IO ()
main = do
  (f: _) <- getArgs
  print . head =<< runResourceT (runT (lineSource f ~> numLines))
