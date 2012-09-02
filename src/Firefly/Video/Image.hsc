--------------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
module Firefly.Video.Image
    ( Image
    , imageFromNoise
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>))
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "video/image.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_imageFromNoise" ff_imageFromNoise
    :: CInt -> CInt -> IO (Ptr CImage)
foreign import ccall "&ff_imageFree" ff_imageFree
    :: FunPtr (Ptr CImage -> IO ())


--------------------------------------------------------------------------------
imageFromNoise :: (Int, Int) -> IO Image
imageFromNoise (w, h) = do
    ptr <- ff_imageFromNoise (fromIntegral w) (fromIntegral h)
    Image <$> newForeignPtr ff_imageFree ptr
