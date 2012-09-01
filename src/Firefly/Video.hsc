{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video
    ( setMode
    , screenSize
    , frame
    
    , line
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types (CDouble (..), CInt (..))


--------------------------------------------------------------------------------
import           Firefly.Vector


--------------------------------------------------------------------------------
#include <video.h>


--------------------------------------------------------------------------------
foreign import ccall unsafe "setMode" c_setMode
    :: CInt -> CInt -> IO ()


--------------------------------------------------------------------------------
setMode :: (Int, Int) -> IO ()
setMode (width, height) =
    c_setMode (fromIntegral width) (fromIntegral height)


--------------------------------------------------------------------------------
foreign import ccall unsafe "screenWidth" c_screenWidth :: IO CInt
foreign import ccall unsafe "screenHeight" c_screenHeight :: IO CInt


--------------------------------------------------------------------------------
screenSize :: IO (Int, Int)
screenSize = do
    w <- c_screenWidth
    h <- c_screenHeight
    return (fromIntegral w, fromIntegral h)


--------------------------------------------------------------------------------
foreign import ccall unsafe "startFrame" c_startFrame :: IO ()
foreign import ccall unsafe "endFrame" c_endFrame :: IO ()


--------------------------------------------------------------------------------
frame :: IO () -> IO ()
frame block = do
    c_startFrame
    block
    c_endFrame


--------------------------------------------------------------------------------
foreign import ccall unsafe "startLine" c_startLine :: IO ()
foreign import ccall unsafe "endLine" c_endLine :: IO ()


--------------------------------------------------------------------------------
line :: [Vector] -> IO ()
line vectors = do
    c_startLine
    mapM_ vertex vectors
    c_endLine


--------------------------------------------------------------------------------
foreign import ccall unsafe "vertex" c_vertex
    :: CDouble -> CDouble -> IO ()


--------------------------------------------------------------------------------
vertex :: Vector -> IO ()
vertex (Vector x y) = c_vertex (realToFrac x) (realToFrac y)
{-# INLINE vertex #-}
