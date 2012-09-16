--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Texture
    ( Texture
    , textureFromGradient
    , textureFromPng

    , textureSize

    , textureSlice
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
#include "firefly/video/texture.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_textureFromGradient" ff_textureFromGradient
    :: CInt -> CInt -> IO (Ptr CTexture)
foreign import ccall unsafe "ff_textureFromPng" ff_textureFromPng
    :: CString -> IO (Ptr CTexture)
foreign import ccall "&ff_textureFree" ff_textureFree
    :: FunPtr (Ptr CTexture -> IO ())
foreign import ccall unsafe "ff_textureSlice" ff_textureSlice
    :: Ptr CTexture -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CTexture)


--------------------------------------------------------------------------------
textureFromGradient :: (Int, Int) -> IO Texture
textureFromGradient (w, h) = do
    ptr <- ff_textureFromGradient (fromIntegral w) (fromIntegral h)
    Texture <$> newForeignPtr ff_textureFree ptr


--------------------------------------------------------------------------------
textureFromPng :: FilePath -> IO Texture
textureFromPng filePath = do
    ptr <- withCString filePath ff_textureFromPng
    if ptr /= nullPtr
        then Texture <$> newForeignPtr ff_textureFree ptr
        else error $
            "Firefly.Video.Texture.textureFromPng: Can't load " ++ show filePath


--------------------------------------------------------------------------------
textureSize :: Texture -> (Int, Int)
textureSize (Texture fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    w <- #{peek ff_texture, width}  ptr :: IO CInt
    h <- #{peek ff_texture, height} ptr :: IO CInt
    return (fromIntegral w, fromIntegral h)


--------------------------------------------------------------------------------
textureSlice :: (Int, Int)  -- ^ (X, Y)
             -> (Int, Int)  -- ^ (Width, Height)
             -> Texture     -- ^ Original image
             -> Texture     -- ^ Sliced image
textureSlice (x, y) (w, h) (Texture fptr) = unsafePerformIO $
    withForeignPtr fptr $ \ptr -> do
        ptr' <- ff_textureSlice ptr
            (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
        Texture <$> newForeignPtr ff_textureFree ptr'
