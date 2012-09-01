{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( Key
    
    , flush

    , quit
    , keyDown
    , mousePosition
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types        (CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include "input.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "input_flush" input_flush :: IO ()
foreign import ccall unsafe "input_quit" input_quit :: IO CInt
foreign import ccall unsafe "input_keyDown" input_keyDown :: CInt -> IO CInt
foreign import ccall unsafe "input_mouseX" input_mouseX :: IO CInt
foreign import ccall unsafe "input_mouseY" input_mouseY :: IO CInt


--------------------------------------------------------------------------------
flush :: IO ()
flush = input_flush


--------------------------------------------------------------------------------
quit :: IO Bool
quit = do
    i <- input_quit
    return $ i /= 0


--------------------------------------------------------------------------------
keyDown :: Key -> IO Bool
keyDown (Key code) = do
    i <- input_keyDown code
    return $ i /= 0


--------------------------------------------------------------------------------
mousePosition :: IO (Int, Int)
mousePosition = do
    x <- input_mouseX
    y <- input_mouseY
    return (fromIntegral x, fromIntegral y)
