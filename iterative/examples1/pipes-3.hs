{-# LANGUAGE BangPatterns #-}
import Pipes
import Pipes.Prelude as P
import Data.Char
import System.IO
import System.Environment
import Control.Applicative
import Control.Exception
import qualified Control.Foldl as Foldl
import Data.Monoid

numLines :: MonadPlus m => Pipe String Int m ()
numLines = go (0::Int) where
  go x = (continue x) <|> (yield x)
  continue x = do
    y <- await
    continue (x+(Prelude.length (Prelude.filter (=='\n') y)))


numChars :: Monad m => Producer String m () -> m Int
numChars = P.fold go 0 id
  where go i s = i + Prelude.length s

main :: IO ()
main = do
  (f:_) <- getArgs
  Prelude.print =<< bracket (openFile f ReadMode)
                            (hClose)
                            (\hdl -> runEffect $ Foldl.purely fold ((,) <$> Foldl.length
                                                                        <*> Foldl.foldMap (Sum . Prelude.length) getSum) $ fromHandle hdl)
