--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video.Font
    ( Font
    , fontFromTtf
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
#include "video/font.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_fontFromTtf" ff_fontFromTtf
    :: CString -> CInt -> IO (Ptr CFont)
foreign import ccall unsafe "&ff_fontFree" ff_fontFree
    :: FunPtr (Ptr CFont -> IO ())


--------------------------------------------------------------------------------
fontFromTtf :: FilePath -> CInt -> IO Font
fontFromTtf filePath size = do
    ptr <- withCString filePath $ \cstr ->
        ff_fontFromTtf cstr (fromIntegral size)
    if ptr /= nullPtr
        then Font <$> newForeignPtr ff_fontFree ptr
        else error $
            "Firefly.Video.Font.fontFromTtf: Can't load " ++ show filePath
