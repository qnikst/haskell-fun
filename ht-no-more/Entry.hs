-- We can call this module Main due to
--
-- @
-- Main.hs:1:1: error:
--    The IO action ‘main’ is not defined in module ‘Main’
-- @
module Entry where

import Control.Concurrent

entry :: IO ()
entry = print =<< getNumCapabilities
