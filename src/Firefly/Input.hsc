{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( Key
    
    , flushInput

    , hasReceivedQuit
    , isKeyDown
    , getMousePosition
    , isMouseButtonDown
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types        (CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include "input.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_flushInput" ff_flushInput :: IO ()
foreign import ccall unsafe "ff_hasReceivedQuit" ff_hasReceivedQuit
    :: IO CInt
foreign import ccall unsafe "ff_isKeyDown" ff_isKeyDown :: CInt -> IO CInt
foreign import ccall unsafe "ff_getMouseX" ff_getMouseX :: IO CInt
foreign import ccall unsafe "ff_getMouseY" ff_getMouseY :: IO CInt
foreign import ccall unsafe "ff_isMouseButtonDown" ff_isMouseButtonDown
    :: CInt -> IO CInt


--------------------------------------------------------------------------------
flushInput :: IO ()
flushInput = ff_flushInput
{-# INLINE flushInput #-}


--------------------------------------------------------------------------------
hasReceivedQuit :: IO Bool
hasReceivedQuit = do
    i <- ff_hasReceivedQuit
    return $ i /= 0
{-# INLINE hasReceivedQuit #-}


--------------------------------------------------------------------------------
isKeyDown :: Key -> IO Bool
isKeyDown (Key code) = do
    i <- ff_isKeyDown code
    return $ i /= 0
{-# INLINE isKeyDown #-}


--------------------------------------------------------------------------------
getMousePosition :: IO (Int, Int)
getMousePosition = do
    x <- ff_getMouseX
    y <- ff_getMouseY
    return (fromIntegral x, fromIntegral y)
{-# INLINE getMousePosition #-}


--------------------------------------------------------------------------------
isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown (MouseButton code) = do
    i <- ff_isMouseButtonDown code
    return $ i /= 0
{-# INLINE isMouseButtonDown #-}
