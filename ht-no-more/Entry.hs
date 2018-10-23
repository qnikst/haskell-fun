-- We can call this module Main due to
--
-- @
-- Main.hs:1:1: error:
--    The IO action ‘main’ is not defined in module ‘Main’
-- @
module Entry where

import Control.Concurrent
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Function
import System.IO
import qualified Data.ByteString as BS

entry :: IO ()
entry = do
    print =<< getNumCapabilities
    h <- openFile "big_file" ReadMode
    fix $ \loop -> do
      bs <- BS.hGetSome h defaultChunkSize
      if BS.null bs
      then pure ()
      else do
        print bs
        loop
