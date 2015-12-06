{-# LANGUAGE ForeignFunctionInterface #-}
import Criterion.Main

import Foreign.C
import Control.Concurrent.Async

foreign import ccall safe "<unistd.h> getpid" safe_ppid :: IO CInt
foreign import ccall unsafe "<unistd.h> getpid" unsafe_ppid :: IO CInt

parSafe, parUnsafe :: IO [CInt]
parSafe = mapConcurrently (const safe_ppid) [0..1000]
parUnsafe = mapConcurrently (const unsafe_ppid) [0..1000]

main = defaultMain
   [ bench "safe" $ nfIO safe_ppid
   , bench "unsafe" $ nfIO unsafe_ppid
   , bench "par-safe" $ nfIO parSafe
   , bench "par-unsafe" $ nfIO parUnsafe
   ]



