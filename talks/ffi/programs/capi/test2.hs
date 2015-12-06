{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign
import Foreign.C

foreign import ccall "pi.h &pi" c_pi :: Ptr CDouble
-- Impossible:
-- foreign import ccall "pi.h &pi2" c_pi2 :: Ptr CDouble

main = do
  print (c_pi)
