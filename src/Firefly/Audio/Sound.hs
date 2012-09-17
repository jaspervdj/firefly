--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Audio.Sound
    ( Sound
    , soundFromFile

    , soundFilePath
    ) where



--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe       (unsafePerformIO)


--------------------------------------------------------------------------------
import           Firefly.Audio.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_soundFromFile" ff_soundFromFile
    :: CString -> IO (Ptr CSound)
foreign import ccall "&ff_soundFree" ff_soundFree
    :: FunPtr (Ptr CSound -> IO ())
foreign import ccall unsafe "ff_soundFilePath" ff_soundFilePath
    :: Ptr CSound -> IO (Ptr CChar)


--------------------------------------------------------------------------------
-- | Load a sound sample from a file. Supported formats depend on your SDL_mixer
-- version, WAV is probably the safest bet, but OGG should also be fine.
soundFromFile :: FilePath -> IO Sound
soundFromFile filePath = do
    ptr <- withCString filePath ff_soundFromFile
    if ptr /= nullPtr
        then Sound <$> newForeignPtr ff_soundFree ptr
        else error $
            "Firefly.Audio.Sound.soundFromFile: Can't load " ++ show filePath


--------------------------------------------------------------------------------
soundFilePath :: Sound -> FilePath
soundFilePath (Sound fptr) = unsafePerformIO $ withForeignPtr fptr $ \ptr ->
    ff_soundFilePath ptr >>= peekCString
