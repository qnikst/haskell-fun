{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Data.Conduit
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import System.Environment

numLines :: Monad m => Sink a m Int
numLines = CL.fold go 0 where
  go !x _ = x+1

numChars :: Monad m => Sink ByteString m Int
numChars = CL.fold go 0 where
  go !x s = x + BS8.length s

main = do
  (f:_) <- getArgs
  print =<<  (runResourceT $ CB.sourceFile f $= CB.lines $$ getZipSink ((,) <$> ZipSink numLines <*> ZipSink numChars))
