newtype {-# CTYPE "const char*" #-} T = T CString

foreign import ccall "spiel/spiel.h m0_spiel_start"
  c_spiel_start :: Ptr SpielContextV
                  -> Ptr ReqHV
                                  -> Ptr T
                                                  -> CString
                                                                  -> IO CInt
