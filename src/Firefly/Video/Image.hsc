--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Image
    ( Image
    , imageFromGradient
    , imageFromPng

    , imageSize

    , imageSlice
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe       (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "firefly/video/image.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_imageFromGradient" ff_imageFromGradient
    :: CInt -> CInt -> IO (Ptr CImage)
foreign import ccall unsafe "ff_imageFromPng" ff_imageFromPng
    :: CString -> IO (Ptr CImage)
foreign import ccall "&ff_imageFree" ff_imageFree
    :: FunPtr (Ptr CImage -> IO ())
foreign import ccall unsafe "ff_imageSlice" ff_imageSlice
    :: Ptr CImage -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CImage)


--------------------------------------------------------------------------------
imageFromGradient :: (Int, Int) -> IO Image
imageFromGradient (w, h) = do
    ptr <- ff_imageFromGradient (fromIntegral w) (fromIntegral h)
    Image <$> newForeignPtr ff_imageFree ptr


--------------------------------------------------------------------------------
imageFromPng :: FilePath -> IO Image
imageFromPng filePath = do
    ptr <- withCString filePath ff_imageFromPng
    if ptr /= nullPtr
        then Image <$> newForeignPtr ff_imageFree ptr
        else error $
            "Firefly.Video.Image.imageFromPng: Can't load " ++ show filePath


--------------------------------------------------------------------------------
imageSize :: Image -> (Int, Int)
imageSize (Image fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    w <- #{peek ff_image, width}  ptr :: IO CInt
    h <- #{peek ff_image, height} ptr :: IO CInt
    return (fromIntegral w, fromIntegral h)


--------------------------------------------------------------------------------
imageSlice :: (Int, Int)  -- ^ (X, Y)
           -> (Int, Int)  -- ^ (Width, Height)
           -> Image       -- ^ Original image
           -> Image       -- ^ Sliced image
imageSlice (x, y) (w, h) (Image fptr) = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
        ptr' <- ff_imageSlice ptr
            (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
        Image <$> newForeignPtr ff_imageFree ptr'
