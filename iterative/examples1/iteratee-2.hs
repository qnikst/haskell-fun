-- Derived from John Lato examples:
-- https://github.com/JohnLato/iteratee/blob/master/Examples/word.hs
{-# LANGUAGE BangPatterns #-}
import Data.Iteratee
import qualified Data.Iteratee as I
import Data.Iteratee.IO
import qualified Data.ByteString.Char8 as BC

import Data.Char (ord)
import Data.ListLike as LL
import System.Environment (getArgs)
import System.IO

-- | An efficient numLines using the foldl' iteratee.
-- Rather than converting a stream, this simply counts newline characters.
numLines :: Monad m => I.Iteratee BC.ByteString m Int
numLines = I.foldl' step 0
 where
     step !acc el = if el == (fromIntegral $ ord '\n') then acc + 1 else acc

main = do
  (fname:_) <- getArgs
  withFile fname ReadMode $ \hdl1 ->
    withFile fname ReadMode $ \hdl2 -> do
      print =<< run =<< ((enumHandle 65536 hdl1 >> enumHandle 65536 hdl2) numLines)
