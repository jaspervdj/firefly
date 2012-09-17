--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Font
    ( Font
    , fontFromTtf
    , fontSize
    , fontStringWidth
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
foreign import ccall unsafe "ff_fontFromTtf" ff_fontFromTtf
    :: CString -> CInt -> IO (Ptr CFont)
foreign import ccall unsafe "&ff_fontFree" ff_fontFree
    :: FunPtr (Ptr CFont -> IO ())
foreign import ccall unsafe "ff_fontSize" ff_fontSize
    :: Ptr CFont -> IO CInt
foreign import ccall unsafe "ff_fontStringWidth" ff_fontStringWidth
    :: Ptr CFont -> Ptr CULong -> CInt -> IO CDouble


--------------------------------------------------------------------------------
-- | Load a TTF font in a certain size.
fontFromTtf :: FilePath -> Int -> IO Font
fontFromTtf filePath size = do
    ptr <- withCString filePath $ \cstr ->
        ff_fontFromTtf cstr (fromIntegral size)
    if ptr /= nullPtr
        then Font <$> newForeignPtr ff_fontFree ptr
        else error $
            "Firefly.Video.Font.fontFromTtf: Can't load " ++ show filePath


--------------------------------------------------------------------------------
-- | Get the size of a font
fontSize :: Font -> Int
fontSize (Font fptr) = unsafePerformIO $ do
    size <- withForeignPtr fptr ff_fontSize
    return $ fromIntegral size
{-# INLINE fontSize #-}


--------------------------------------------------------------------------------
-- | Calculate the width of the string if we were to draw it in the font.
-- This can be useful if you want to center or justify text.
fontStringWidth :: Font -> String -> Double
fontStringWidth (Font fptr) string = unsafePerformIO $ do
    width <- withForeignPtr fptr $ withUnicode string . ff_fontStringWidth
    return $ realToFrac width
{-# INLINE fontStringWidth #-}
