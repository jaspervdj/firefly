--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Image
    ( Image
    , imageCreate
    , imageFromPng

    , imageSize
    , imageBpp
    , imageWithPixels

    , imageSlice
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe       (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_imageCreate" ff_imageCreate
    :: CInt -> CInt -> CInt -> IO (Ptr CImage)
foreign import ccall unsafe "ff_imageFromPng" ff_imageFromPng
    :: CString -> IO (Ptr CImage)
foreign import ccall "&ff_imageFree" ff_imageFree
    :: FunPtr (Ptr CImage -> IO ())
foreign import ccall unsafe "ff_imageWidth" ff_imageWidth
    :: Ptr CImage -> IO CInt
foreign import ccall unsafe "ff_imageHeight" ff_imageHeight
    :: Ptr CImage -> IO CInt
foreign import ccall unsafe "ff_imageBpp" ff_imageBpp
    :: Ptr CImage -> IO CInt
foreign import ccall unsafe "ff_imagePixels" ff_imagePixels
    :: Ptr CImage -> IO (Ptr CUChar)
foreign import ccall unsafe "ff_imageSlice" ff_imageSlice
    :: Ptr CImage -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CImage)


--------------------------------------------------------------------------------
-- | Low-level method to create images
imageCreate :: (Int, Int)  -- ^ (Width, Height)
            -> Int         -- ^ Bytes per pixel
            -> IO Image    -- ^ Resulting image
imageCreate (w, h) b = do
    ptr <- ff_imageCreate (fromIntegral w) (fromIntegral h) (fromIntegral b)
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
    w <- ff_imageWidth  ptr
    h <- ff_imageHeight ptr
    return (fromIntegral w, fromIntegral h)
{-# INLINE imageSize #-}


--------------------------------------------------------------------------------
imageBpp :: Image -> Int
imageBpp (Image fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    bpp <- ff_imageBpp ptr
    return (fromIntegral bpp)
{-# INLINE imageBpp #-}


--------------------------------------------------------------------------------
-- | Perform a calculation on the pixeldata. You should not use the 'Ptr'
-- outside of this context, since the 'Image' memory might be released by then.
imageWithPixels :: Image -> (Ptr CUChar -> IO a) -> IO a
imageWithPixels (Image fptr) f = withForeignPtr fptr $ \ptr ->
    ff_imagePixels ptr >>= f


--------------------------------------------------------------------------------
imageSlice :: (Int, Int)  -- ^ (X, Y)
           -> (Int, Int)  -- ^ (Width, Height)
           -> Image       -- ^ Original image
           -> Image       -- ^ Sliced image
imageSlice (x, y) (w, h) (Image fptr) = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
        ptr' <- ff_imageSlice ptr (fromIntegral x) (fromIntegral y)
            (fromIntegral w) (fromIntegral h)
        Image <$> newForeignPtr ff_imageFree ptr'
