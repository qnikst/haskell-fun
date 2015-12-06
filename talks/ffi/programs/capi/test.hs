{-# LANGUAGE CApiFFI #-}

foreign import capi "pi.h value pi" c_pi :: Double
foreign import capi "pi.h value pi2" c_pi2 :: Double
foreign import capi "pi.h test" c_test :: Int -> IO Int

main = do
  print =<< c_test 7 -- (c_pi, c_pi2)
