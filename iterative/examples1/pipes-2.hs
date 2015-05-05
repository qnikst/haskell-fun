{-# LANGUAGE BangPatterns #-}
import Pipes
import Pipes.Prelude as P
import Data.Char
import System.IO
import System.Environment
import Control.Applicative
import Control.Exception
import Control.Monad.Managed

main :: IO ()
main = do
  (f1:f2:_) <- getArgs
  runManaged $ do
    hdl1 <- managed $ withFile f1 ReadMode
    hdl2 <- managed $ withFile f2 ReadMode
    liftIO . Prelude.print =<< runEffect (P.length (fromHandle hdl1 >> fromHandle hdl2))
