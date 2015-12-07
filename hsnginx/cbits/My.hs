{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module My where

import Foreign.Ptr
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array


foreign export ccall fac :: Int -> Int
foreign export ccall reverseIt :: Ptr Pool -> CString -> Int -> IO (Ptr NgxString)

fac 0 = 1
fac i = i * fac (i-1)

data Pool = Pool

foreign import ccall unsafe "core/ngx_palloc.h ngx_pcalloc"
  ngx_pcalloc :: Ptr Pool -> CSize -> IO (Ptr a)

newtype NgxString = NgxString ()

reverseIt :: Ptr Pool -> CString -> Int -> IO (Ptr NgxString)
reverseIt pool s l = do
  s <- peekCStringLen (s, l)
  withCStringLen (reverse s) $ \(ptr, len) -> do
    cptr <- ngx_pcalloc pool (fromIntegral len)
    copyArray cptr ptr len
    return (castPtr cptr)


