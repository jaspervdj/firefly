--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Image
    ( Image
    , imageFromGradient

    , imageSize
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "video/image.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_imageFromGradient" ff_imageFromGradient
    :: CInt -> CInt -> IO (Ptr CImage)
foreign import ccall "&ff_imageFree" ff_imageFree
    :: FunPtr (Ptr CImage -> IO ())


--------------------------------------------------------------------------------
imageFromGradient :: (Int, Int) -> IO Image
imageFromGradient (w, h) = do
    ptr <- ff_imageFromGradient (fromIntegral w) (fromIntegral h)
    Image <$> newForeignPtr ff_imageFree ptr


--------------------------------------------------------------------------------
imageSize :: Image -> (Int, Int)
imageSize (Image fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    w <- #{peek ff_image, width}  ptr :: IO CInt
    h <- #{peek ff_image, height} ptr :: IO CInt
    return (fromIntegral w, fromIntegral h)
