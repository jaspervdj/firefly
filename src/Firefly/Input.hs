--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Input
    ( Key
    , MouseButton

    , setGrabInput
    , isGrabInput

    , flushInput

    , hasReceivedQuit

    , isKeyDown
    , isKeyPressed
    , isKeyReleased

    , isMouseButtonDown
    , isMouseButtonPressed
    , isMouseButtonReleased
    , getMousePosition
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types        (CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Internal
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_setGrabInput" ff_setGrabInput
    :: CInt -> IO ()
foreign import ccall unsafe "ff_isGrabInput" ff_isGrabInput
    :: IO CInt
foreign import ccall unsafe "ff_flushInput" ff_flushInput
    :: IO ()
foreign import ccall unsafe "ff_hasReceivedQuit" ff_hasReceivedQuit
    :: IO CInt
foreign import ccall unsafe "ff_isKeyDown" ff_isKeyDown
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_isKeyPressed" ff_isKeyPressed
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_isKeyReleased" ff_isKeyReleased
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_isMouseButtonDown" ff_isMouseButtonDown
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_isMouseButtonPressed" ff_isMouseButtonPressed
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_isMouseButtonReleased" ff_isMouseButtonReleased
    :: CInt -> IO CInt
foreign import ccall unsafe "ff_getMouseX" ff_getMouseX
    :: IO CInt
foreign import ccall unsafe "ff_getMouseY" ff_getMouseY
    :: IO CInt


--------------------------------------------------------------------------------
setGrabInput :: Bool -> IO ()
setGrabInput = ff_setGrabInput . fromBool
{-# INLINE setGrabInput #-}


--------------------------------------------------------------------------------
isGrabInput :: IO Bool
isGrabInput = fmap toBool ff_isGrabInput
{-# INLINE isGrabInput #-}


--------------------------------------------------------------------------------
-- | Process all pending events. You should call this each step of your main
-- game loop, just before processing the input using the other functions in this
-- module.
flushInput :: IO ()
flushInput = ff_flushInput
{-# INLINE flushInput #-}


--------------------------------------------------------------------------------
-- | Did the user close the window?
hasReceivedQuit :: IO Bool
hasReceivedQuit = fmap toBool ff_hasReceivedQuit
{-# INLINE hasReceivedQuit #-}


--------------------------------------------------------------------------------
-- | Is the key being held down?
isKeyDown :: Key -> IO Bool
isKeyDown (Key code) = fmap toBool (ff_isKeyDown code)
{-# INLINE isKeyDown #-}


--------------------------------------------------------------------------------
isKeyPressed :: Key -> IO Bool
isKeyPressed (Key code) = fmap toBool (ff_isKeyPressed code)
{-# INLINE isKeyPressed #-}


--------------------------------------------------------------------------------
isKeyReleased :: Key -> IO Bool
isKeyReleased (Key code) = fmap toBool (ff_isKeyReleased code)
{-# INLINE isKeyReleased #-}


--------------------------------------------------------------------------------
-- | Is the mouse button being held down?
isMouseButtonDown :: MouseButton -> IO Bool
isMouseButtonDown (MouseButton code) =
    fmap toBool (ff_isMouseButtonDown code)
{-# INLINE isMouseButtonDown #-}


--------------------------------------------------------------------------------
isMouseButtonPressed :: MouseButton -> IO Bool
isMouseButtonPressed (MouseButton code) =
    fmap toBool (ff_isMouseButtonPressed code)
{-# INLINE isMouseButtonPressed #-}


--------------------------------------------------------------------------------
isMouseButtonReleased :: MouseButton -> IO Bool
isMouseButtonReleased (MouseButton code) =
    fmap toBool (ff_isMouseButtonReleased code)
{-# INLINE isMouseButtonReleased #-}


--------------------------------------------------------------------------------
-- | Obtain the current mouse position
getMousePosition :: IO (Int, Int)
getMousePosition = do
    x <- ff_getMouseX
    y <- ff_getMouseY
    return (fromIntegral x, fromIntegral y)
{-# INLINE getMousePosition #-}
