--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Audio
    ( Loop (..)
    , playMusic
    , playSound
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
import           Firefly.Audio.Internal


--------------------------------------------------------------------------------
#include "firefly/audio.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_playMusic" ff_playMusic
    :: CString -> CInt -> IO ()
foreign import ccall unsafe "ff_playSound" ff_playSound
    :: Ptr CSound -> IO ()


--------------------------------------------------------------------------------
-- | Loop mode for 'playMusic'
data Loop
    = Loop Int     -- ^ Loop N times
    | DontLoop     -- ^ Just play once
    | LoopForever  -- ^ Loop until the world ends
    deriving (Show)


--------------------------------------------------------------------------------
fromLoop :: Loop -> CInt
fromLoop (Loop n)    = fromIntegral n
fromLoop DontLoop    = 1
fromLoop LoopForever = -1


--------------------------------------------------------------------------------
-- | Play some background music. This stops the previous background music (if
-- any) from playing.
--
-- Supported formats depend on your platform and version of SDL_mixer, but WAV,
-- OGG and MP3 should be a safe bet.
--
-- This call is non-blocking.
playMusic :: FilePath -> Loop -> IO ()
playMusic filePath l = withCString filePath $ \str ->
    ff_playMusic str (fromLoop l)


--------------------------------------------------------------------------------
-- Play some sound.
--
-- This call is non-blocking.
playSound :: Sound -> IO ()
playSound (Sound fptr) = withForeignPtr fptr ff_playSound
