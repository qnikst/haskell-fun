{-# LANGUAGE GHCForeignImportPrim     #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE UnliftedFFITypes #-}
import Criterion.Main

import Foreign
import Foreign.C
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import GHC.Ptr
import GHC.Prim
import GHC.Int
import GHC.Word
import GHC.Types

import System.IO.Unsafe (unsafeDupablePerformIO)

foreign import ccall unsafe "in_word_set" c_in_word_set :: CString -> CInt -> IO CString

foreign import prim "in_word_setprim1" prim_in_word_set# :: Addr# -> Int# -> Addr#

inWordsSet :: ByteString -> IO Bool
inWordsSet str = unsafeUseAsCStringLen str $ \(cptr, len) -> do
  result <- c_in_word_set cptr (fromIntegral len)
  return $ nullPtr /= result

sumsets1 :: [ByteString] -> IO Int
sumsets1 = fmap sum . mapM (fmap fromEnum . inWordsSet)

sumsets2 :: [ByteString] -> IO Int
sumsets2 = fmap sum . mapM (fmap fromEnum . inWordsSet1)

inWordsSet1 :: ByteString -> IO Bool
inWordsSet1 str = unsafeUseAsCStringLen str $ \(Ptr addr, I# len) -> do
    case prim_in_word_set# addr len of
      result -> case neAddr# nullAddr# result of
                  0# -> return False
                  _  -> return True

unsafeContents :: ByteString -> (CString, Int)
unsafeContents str = unsafeDupablePerformIO $ unsafeUseAsCStringLen str return

inWordsSet1# :: ByteString -> Int#
inWordsSet1# str = case unsafeContents str of
    (Ptr addr, I# len) -> case prim_in_word_set# addr len of
      result -> neAddr# nullAddr# result

sumsetsopt :: [ByteString] -> Int
sumsetsopt xs = let loop# [] c = I#  c
                    loop# (y:ys) c = loop# ys (c +# (inWordsSet1# y))
                in loop# xs 0#


wrds =
  [ "abbreviating"
  , "abbreviation"
  , "abbreviations"
  , "a"
  , "abberiations"
  ,"a"
  ,"aah"
  ,"aahed"
  ,"aahing"
  ,"aahs"
  ,"aardvark"
  ,"aardvarks"
  ,"aardwolf"
  ,"ab"
  ,"abaci"
  ,"aback"
  ,"abacus"
  , "sdfsdfs"
  , "sdfsdfs"
  ,"abacuses"
  ,"abaft"
  ,"abalone"
  ,"abalones"
  ,"abandon"
  ,"abandoned"
  ,"abandonedly"
  ,"abandonee"
  ,"abandoner"
  ,"abandoners"
  ,"abandoning"
  ,"abandonment"
  ,"abandonments"
  ,"abandons"
  ,"abase"
  ,"abased"
  ,"abasedly"
  ,"abasement"
  ,"abaser"
  ,"abasers"
  ,"abases"
  ,"abash"
  ,"abashed"
  ,"abashedly"
  ,"abashes"
  ,"abashing"
  ,"abashment"
  ,"abashments"
  ,"abasing"
  ,"abatable"
  ,"abate"
  ,"abated"
  ,"abatement"
  ,"abatements"
  ,"abater"
  ,"abaters"
  ,"abates"
  ,"abating"
  ]

main = -- do print =<< sumsets1 wrds
       --   print =<< sumsets2 wrds
  defaultMain
  [ bench "inWordsSet"  $ nfIO $ and <$> mapM inWordsSet wrds
  , bench "inWordsSet1" $ nfIO $ and <$> mapM inWordsSet1 wrds
  , bench "sum-unsafe"  $ nfIO $ sumsets1 wrds
  , bench "sum-prim"    $ nfIO $ sumsets2 wrds
  , bench "sum-opt"     $ nf sumsetsopt wrds
  ]
