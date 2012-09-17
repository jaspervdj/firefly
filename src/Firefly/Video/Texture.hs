--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Texture
    ( Texture
    , textureFromImage
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
import           System.IO.Unsafe       (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_textureFromImage" ff_textureFromImage
    :: Ptr CImage -> IO (Ptr CTexture)
foreign import ccall unsafe "ff_textureFromPng" ff_textureFromPng
    :: CString -> IO (Ptr CTexture)
foreign import ccall "&ff_textureFree" ff_textureFree
    :: FunPtr (Ptr CTexture -> IO ())
foreign import ccall unsafe "ff_textureWidth" ff_textureWidth
    :: Ptr CTexture -> IO CInt
foreign import ccall unsafe "ff_textureHeight" ff_textureHeight
    :: Ptr CTexture -> IO CInt
foreign import ccall unsafe "ff_textureSlice" ff_textureSlice
    :: Ptr CTexture -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CTexture)


--------------------------------------------------------------------------------
textureFromImage :: Image -> IO Texture
textureFromImage (Image ifptr) = withForeignPtr ifptr $ \iptr -> do
    tptr <- ff_textureFromImage iptr
    Texture <$> newForeignPtr ff_textureFree tptr


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
    w <- ff_textureWidth  ptr
    h <- ff_textureHeight ptr
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
