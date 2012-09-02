--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Image
    ( Image
    , imageFromGradient
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr


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
