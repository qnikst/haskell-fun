{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Monoid
import Data.Machine
import System.IO

import Control.Monad ( unless )
import System.Environment ( getArgs )
import qualified Control.Foldl as L

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

main :: IO ()
main = do
  (f: _) <- getArgs
  print . L.fold ((,) <$> L.length <*> (L.foldMap (Sum . length) getSum)) =<< runResourceT (runT $ lineSource f)
