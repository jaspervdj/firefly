{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video
    ( setVideoMode
    , getScreenSize
    , frame

    , drawLine

    , drawImage
    , drawImageCentered
    , drawImageDebug

    , drawString

    , pushMatrix
    , translate
    , rotate
    , scale

    , pushColor
    , setColor
    , getColor
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable


--------------------------------------------------------------------------------
import           Firefly.Vector
import           Firefly.Video.Color
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "video.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_setVideoMode" ff_setVideoMode
    :: CInt -> CInt -> IO ()
foreign import ccall unsafe "ff_getScreenWidth" ff_getScreenWidth :: IO CInt
foreign import ccall unsafe "ff_getScreenHeight" ff_getScreenHeight :: IO CInt
foreign import ccall unsafe "ff_startFrame" ff_startFrame :: IO ()
foreign import ccall unsafe "ff_endFrame" ff_endFrame :: IO ()
foreign import ccall unsafe "ff_startLine" ff_startLine :: IO ()
foreign import ccall unsafe "ff_endLine" ff_endLine :: IO ()
foreign import ccall unsafe "ff_vertex" ff_vertex
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_drawImage" ff_drawImage
    :: Ptr CImage -> IO ()
foreign import ccall unsafe "ff_drawImageCentered" ff_drawImageCentered
    :: Ptr CImage -> IO ()
foreign import ccall unsafe "ff_drawImageDebug" ff_drawImageDebug
    :: Ptr CImage -> IO ()
foreign import ccall unsafe "ff_drawString" ff_drawString
    :: Ptr CFont -> Ptr CULong -> CInt -> IO ()
foreign import ccall unsafe "ff_pushMatrix" ff_pushMatrix :: IO ()
foreign import ccall unsafe "ff_popMatrix" ff_popMatrix :: IO ()
foreign import ccall unsafe "ff_translate" ff_translate
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_rotate" ff_rotate :: CDouble -> IO ()
foreign import ccall unsafe "ff_scale" ff_scale :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_setColor" ff_setColor
    :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_getColor" ff_getColor
    :: Ptr CDouble -> IO ()


--------------------------------------------------------------------------------
setVideoMode :: (Int, Int) -> IO ()
setVideoMode (width, height) =
    ff_setVideoMode (fromIntegral width) (fromIntegral height)


--------------------------------------------------------------------------------
getScreenSize :: IO (Int, Int)
getScreenSize = do
    w <- ff_getScreenWidth
    h <- ff_getScreenHeight
    return (fromIntegral w, fromIntegral h)
{-# INLINE getScreenSize #-}


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
{-# INLINE drawImage #-}


--------------------------------------------------------------------------------
drawImageCentered :: Image -> IO ()
drawImageCentered (Image fptr) = withForeignPtr fptr ff_drawImageCentered
{-# INLINE drawImageCentered #-}


--------------------------------------------------------------------------------
drawImageDebug :: Image -> IO ()
drawImageDebug (Image fptr) = withForeignPtr fptr ff_drawImageDebug


--------------------------------------------------------------------------------
drawString :: Font -> String -> IO ()
drawString (Font fptr) string =
    withForeignPtr fptr $ \ptr ->
        withUnicode string $ ff_drawString ptr
{-# INLINE drawString #-}


--------------------------------------------------------------------------------
-- | Pushes the current transformation matrix on the stack, executes the given
-- block of code and then pops the matrix again.
pushMatrix :: IO () -> IO ()
pushMatrix block = do
    ff_pushMatrix
    block
    ff_popMatrix
{-# INLINE pushMatrix #-}


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


--------------------------------------------------------------------------------
setColor :: Color -> IO ()
setColor (Color r g b a) = ff_setColor
    (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
{-# INLINE setColor #-}


--------------------------------------------------------------------------------
getColor :: IO Color
getColor = allocaArray 4 $ \ptr -> do
    ff_getColor ptr
    r <- realToFrac <$> peek ptr
    g <- realToFrac <$> peekElemOff ptr 1
    b <- realToFrac <$> peekElemOff ptr 2
    a <- realToFrac <$> peekElemOff ptr 3
    return $ Color r g b a
{-# INLINE getColor #-}


--------------------------------------------------------------------------------
-- | Pushes the current color on a stack, execute the given block of code and
-- pops the color again.
pushColor :: IO () -> IO ()
pushColor block = do
    color <- getColor
    block
    setColor color
{-# INLINE pushColor #-}
