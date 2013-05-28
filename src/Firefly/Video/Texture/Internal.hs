--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Texture.Internal
    ( CTexture
    , Texture (..)

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
foreign import ccall unsafe "ff_textureId" ff_textureId
    :: Ptr CTexture -> IO CInt
foreign import ccall unsafe "ff_textureWidth" ff_textureWidth
    :: Ptr CTexture -> IO CInt
foreign import ccall unsafe "ff_textureHeight" ff_textureHeight
    :: Ptr CTexture -> IO CInt
foreign import ccall unsafe "ff_textureSlice" ff_textureSlice
    :: Ptr CTexture -> CInt -> CInt -> CInt -> CInt -> IO (Ptr CTexture)


--------------------------------------------------------------------------------
type CTexture = CChar


--------------------------------------------------------------------------------
newtype Texture = Texture (ForeignPtr CTexture)


--------------------------------------------------------------------------------
instance Eq Texture where
    x == y = textureId x == textureId y


--------------------------------------------------------------------------------
instance Ord Texture where
    compare x y = compare (textureId x) (textureId y)


--------------------------------------------------------------------------------
instance Show Texture where
    show t = "(Texture " ++
        "id=" ++ show (textureId t) ++ ", " ++
        "size=" ++ show (textureSize t) ++ ")"


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
textureId :: Texture -> Int
textureId (Texture fptr) = unsafePerformIO $ withForeignPtr fptr $
    fmap fromIntegral . ff_textureId


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
