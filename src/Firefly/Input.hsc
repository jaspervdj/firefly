{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( Key
    
    , flush

    , isQuit
    , isKeyDown
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types        (CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include "input.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "input_flush" input_flush :: IO ()
foreign import ccall unsafe "input_isQuit" input_isQuit :: IO CInt
foreign import ccall unsafe "input_isKeyDown" input_isKeyDown :: CInt -> IO CInt


--------------------------------------------------------------------------------
flush :: IO ()
flush = input_flush


--------------------------------------------------------------------------------
isQuit :: IO Bool
isQuit = do
    i <- input_isQuit
    return $ i /= 0


--------------------------------------------------------------------------------
isKeyDown :: Key -> IO Bool
isKeyDown (Key code) = do
    i <- input_isKeyDown code
    return $ i /= 0
