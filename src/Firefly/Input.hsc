{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( Key
    
    , flushInput

    , receivedQuit
    , keyDown
    , mousePosition
    , mouseButtonDown
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types        (CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include "input.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_flushInput" ff_flushInput :: IO ()
foreign import ccall unsafe "ff_receivedQuit" ff_receivedQuit :: IO CInt
foreign import ccall unsafe "ff_keyDown" ff_keyDown :: CInt -> IO CInt
foreign import ccall unsafe "ff_mouseX" ff_mouseX :: IO CInt
foreign import ccall unsafe "ff_mouseY" ff_mouseY :: IO CInt
foreign import ccall unsafe "ff_mouseButtonDown" ff_mouseButtonDown
    :: CInt -> IO CInt


--------------------------------------------------------------------------------
flushInput :: IO ()
flushInput = ff_flushInput


--------------------------------------------------------------------------------
receivedQuit :: IO Bool
receivedQuit = do
    i <- ff_receivedQuit
    return $ i /= 0


--------------------------------------------------------------------------------
keyDown :: Key -> IO Bool
keyDown (Key code) = do
    i <- ff_keyDown code
    return $ i /= 0


--------------------------------------------------------------------------------
mousePosition :: IO (Int, Int)
mousePosition = do
    x <- ff_mouseX
    y <- ff_mouseY
    return (fromIntegral x, fromIntegral y)


--------------------------------------------------------------------------------
mouseButtonDown :: MouseButton -> IO Bool
mouseButtonDown (MouseButton code) = do
    i <- ff_mouseButtonDown code
    return $ i /= 0
