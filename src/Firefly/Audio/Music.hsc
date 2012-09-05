--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Audio.Music
    ( Music
    , musicFromFile

    , musicFilePath
    ) where



--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe       (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Audio.Internal


--------------------------------------------------------------------------------
#include "firefly/audio/music.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_musicFromFile" ff_musicFromFile
    :: CString -> IO (Ptr CMusic)
foreign import ccall "&ff_musicFree" ff_musicFree
    :: FunPtr (Ptr CMusic -> IO ())


--------------------------------------------------------------------------------
musicFromFile :: FilePath -> IO Music
musicFromFile filePath = do
    ptr <- withCString filePath ff_musicFromFile
    if ptr /= nullPtr
        then Music <$> newForeignPtr ff_musicFree ptr
        else error $
            "Firefly.Audio.Music.musicFromFile: Can't load " ++ show filePath


--------------------------------------------------------------------------------
musicFilePath :: Music -> FilePath
musicFilePath (Music fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    cstring <- #{peek ff_music, filePath}  ptr :: IO CString
    peekCString cstring
