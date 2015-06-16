import Control.Applicative
import Data.Machine
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

prepare :: Machine (Is ByteString) ByteString
prepare = construct (await >>= go) where
  go bs = case BS.length bs of
            0 -> return ()
            1 -> case BS.index bs 0 of
                   0x0D -> let loop = do
                                  mz <- fmap Just await <|> pure Nothing
                                  case mz of
                                    Nothing -> yield sgn
                                    Just bs
                                      | BS.null bs -> loop
                                      | BS.head bs == 0x0A -> yield sgn >> go (BS.drop 1 bs)
                                      | otherwise  -> yield sgn >> go bs
                           in loop
                   _    -> yield bs >> await >>= go
            _ -> case (BS.index bs 0, BS.index bs 1) of
                   (0x0D,0x0A) -> yield sgn >> next (BS.drop 2 bs)
                   (0x0D,_   ) -> yield sgn >> next (BS.drop 1 bs)
                   _           -> do let (pred,post) = BS.span (/=0x0D) bs
                                     yield pred
                                     next post
  next bs
    | BS.null bs = await >>= go
    | otherwise  = go bs
  clean bs
    | BS.null bs = sgn <> bs
    | BS.head bs == 0x0A = bs
  sgn = BS.pack [0x0A]



test = run $ source (map BS.pack [[0,1,2],[3,4,5],[0x0A],[0x0D],[0x0D,0x0A],[1,2,0x0D,3,4],[0x0D],[0x0A,7]]) ~> prepare

