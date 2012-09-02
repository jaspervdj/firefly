{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video
    ( setVideoMode
    , screenSize
    , frame

    , drawLine
    , drawImage
    , drawImageCentered
    , drawImageDebug

    , translate
    , rotate
    , scale
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
import           Firefly.Vector
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "video.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_setVideoMode" ff_setVideoMode
    :: CInt -> CInt -> IO ()
foreign import ccall unsafe "ff_screenWidth" ff_screenWidth :: IO CInt
foreign import ccall unsafe "ff_screenHeight" ff_screenHeight
    :: IO CInt
foreign import ccall unsafe "ff_startFrame" ff_startFrame :: IO ()
foreign import ccall unsafe "ff_endFrame" ff_endFrame :: IO ()
foreign import ccall unsafe "ff_startLine" ff_startLine :: IO ()
foreign import ccall unsafe "ff_endLine" ff_endLine :: IO ()
foreign import ccall unsafe "ff_vertex" ff_vertex
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_drawImage" ff_drawImage
    :: Ptr a -> IO ()
foreign import ccall unsafe "ff_drawImageCentered" ff_drawImageCentered
    :: Ptr a -> IO ()
foreign import ccall unsafe "ff_drawImageDebug" ff_drawImageDebug
    :: Ptr a -> IO ()
foreign import ccall unsafe "ff_translate" ff_translate
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_rotate" ff_rotate :: CDouble -> IO ()
foreign import ccall unsafe "ff_scale" ff_scale :: CDouble -> CDouble -> IO ()


--------------------------------------------------------------------------------
setVideoMode :: (Int, Int) -> IO ()
setVideoMode (width, height) =
    ff_setVideoMode (fromIntegral width) (fromIntegral height)


--------------------------------------------------------------------------------
screenSize :: IO (Int, Int)
screenSize = do
    w <- ff_screenWidth
    h <- ff_screenHeight
    return (fromIntegral w, fromIntegral h)
{-# INLINE screenSize #-}


--------------------------------------------------------------------------------
frame :: IO () -> IO ()
frame block = do
    ff_startFrame
    block
    ff_endFrame
{-# INLINE frame #-}


--------------------------------------------------------------------------------
drawLine :: [Vector] -> IO ()
drawLine vectors = do
    ff_startLine
    mapM_ vertex vectors
    ff_endLine
{-# INLINE drawLine #-}


--------------------------------------------------------------------------------
vertex :: Vector -> IO ()
vertex (Vector x y) = ff_vertex (realToFrac x) (realToFrac y)
{-# INLINE vertex #-}


--------------------------------------------------------------------------------
drawImage :: Image -> IO ()
drawImage (Image fptr) = withForeignPtr fptr ff_drawImage


--------------------------------------------------------------------------------
drawImageCentered :: Image -> IO ()
drawImageCentered (Image fptr) = withForeignPtr fptr ff_drawImageCentered


--------------------------------------------------------------------------------
drawImageDebug :: Image -> IO ()
drawImageDebug (Image fptr) = withForeignPtr fptr ff_drawImageDebug


--------------------------------------------------------------------------------
translate :: Vector -> IO ()
translate (Vector x y) = ff_translate (realToFrac x) (realToFrac y)
{-# INLINE translate #-}


--------------------------------------------------------------------------------
-- | Rotate the transformation matrix by the given amount of /radians/
rotate :: Double -> IO ()
rotate r = ff_rotate (realToFrac r)
{-# INLINE rotate #-}


--------------------------------------------------------------------------------
scale :: Vector -> IO ()
scale (Vector x y) = ff_scale (realToFrac x) (realToFrac y)
{-# INLINE scale #-}
