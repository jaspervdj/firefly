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
#include "video.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "video_setMode" video_setMode :: CInt -> CInt -> IO ()
foreign import ccall unsafe "video_screenWidth" video_screenWidth :: IO CInt
foreign import ccall unsafe "video_screenHeight" video_screenHeight
    :: IO CInt
foreign import ccall unsafe "video_startFrame" video_startFrame :: IO ()
foreign import ccall unsafe "video_endFrame" video_endFrame :: IO ()
foreign import ccall unsafe "video_startLine" video_startLine :: IO ()
foreign import ccall unsafe "video_endLine" video_endLine :: IO ()
foreign import ccall unsafe "video_vertex" video_vertex
    :: CDouble -> CDouble -> IO ()


--------------------------------------------------------------------------------
setMode :: (Int, Int) -> IO ()
setMode (width, height) =
    video_setMode (fromIntegral width) (fromIntegral height)


--------------------------------------------------------------------------------
screenSize :: IO (Int, Int)
screenSize = do
    w <- video_screenWidth
    h <- video_screenHeight
    return (fromIntegral w, fromIntegral h)


--------------------------------------------------------------------------------
frame :: IO () -> IO ()
frame block = do
    video_startFrame
    block
    video_endFrame


--------------------------------------------------------------------------------
line :: [Vector] -> IO ()
line vectors = do
    video_startLine
    mapM_ vertex vectors
    video_endLine


--------------------------------------------------------------------------------
vertex :: Vector -> IO ()
vertex (Vector x y) = video_vertex (realToFrac x) (realToFrac y)
{-# INLINE vertex #-}
