{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( flush

    , isQuit
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types (CInt (..))


--------------------------------------------------------------------------------
#include "input.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "flush" c_flush :: IO ()


--------------------------------------------------------------------------------
flush :: IO ()
flush = c_flush


--------------------------------------------------------------------------------
foreign import ccall unsafe "isQuit" c_isQuit :: IO CInt


--------------------------------------------------------------------------------
isQuit :: IO Bool
isQuit = do
    i <- c_isQuit
    return $ i /= 0
