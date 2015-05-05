{-# LANGUAGE BangPatterns #-}
import Pipes
import Pipes.Prelude as P
import Data.Char
import System.IO
import System.Environment
import Control.Applicative
import Control.Exception

numLines :: MonadPlus m => Pipe String Int m ()
numLines = go (0::Int) where
  go x = (continue x) <|> (yield x)
  continue x = do
    y <- await
    continue (x+(Prelude.length (Prelude.filter (=='\n') y)))

main :: IO ()
main = do
  (f:_) <- getArgs
  Prelude.print =<< bracket (openFile f ReadMode)
                            (hClose)
                            (\hdl -> runEffect $ P.length (fromHandle hdl))
