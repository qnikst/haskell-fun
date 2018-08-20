{-# LANGUAGE ForeignFunctionInterface #-}
-- We can call this module Main due to
--
-- @
-- Main.hs:1:1: error:
--    The IO action ‘main’ is not defined in module ‘Main’
-- @
module Entry where

import Control.Concurrent

foreign export ccall entry :: IO ()

entry :: IO ()
entry = print =<< getNumCapabilities
