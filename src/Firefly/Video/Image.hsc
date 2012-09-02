--------------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
module Firefly.Video.Image
    ( Image
    , imageCreate
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
#include "video/image.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_imageCreate" ff_imageCreate :: IO (Ptr CImage)
foreign import ccall "&ff_imageFree" ff_imageFree
    :: FunPtr (Ptr CImage -> IO ())


--------------------------------------------------------------------------------
type CImage = CChar


--------------------------------------------------------------------------------
newtype Image = Image (ForeignPtr CImage)


--------------------------------------------------------------------------------
imageCreate :: IO Image
imageCreate = do
    ptr <- ff_imageCreate
    Image <$> newForeignPtr ff_imageFree ptr
