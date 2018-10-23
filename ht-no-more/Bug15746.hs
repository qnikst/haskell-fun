import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Function
import System.IO
import qualified Data.ByteString as BS

main :: IO ()
main = do
    h <- openFile "big_file" ReadMode
    fix $ \loop -> do
      bs <- BS.hGetSome h defaultChunkSize
      if BS.null bs
      then pure ()
      else do
        print bs
        loop
