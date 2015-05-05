{-# LANGUAGE BangPatterns #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import System.Environment

numLines :: Monad m => Sink a m Int
numLines = CL.fold go 0 where
  go !x _ = x+1

main = do
  (f:_) <- getArgs
  print =<<  (runResourceT $ CB.sourceFile f $$ CB.lines =$ numLines)
