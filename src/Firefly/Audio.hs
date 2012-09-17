--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Audio
    ( Loop (..)
    , playMusic
    , stopMusic
    , setMusicVolume
    , getMusicVolume

    , playSound
    , playSoundPanning
    , playSoundPosition
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
import           Firefly.Audio.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_playMusic" ff_playMusic
    :: CString -> CInt -> IO ()
foreign import ccall unsafe "ff_stopMusic" ff_stopMusic
    :: IO ()
foreign import ccall unsafe "ff_setMusicVolume" ff_setMusicVolume
    :: CDouble -> IO ()
foreign import ccall unsafe "ff_getMusicVolume" ff_getMusicVolume
    :: IO CDouble
foreign import ccall unsafe "ff_playSound" ff_playSound
    :: Ptr CSound -> IO ()
foreign import ccall unsafe "ff_playSoundPanning" ff_playSoundPanning
    :: Ptr CSound -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_playSoundPosition" ff_playSoundPosition
    :: Ptr CSound -> CDouble -> CDouble -> IO ()


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
-- | Stop the currently playing music without starting a new track.
stopMusic :: IO ()
stopMusic = ff_stopMusic


--------------------------------------------------------------------------------
-- | Set the music volume. Argument must be between 0 and 1.
setMusicVolume :: Double -> IO ()
setMusicVolume v = ff_setMusicVolume (realToFrac v)
{-# INLINE setMusicVolume #-}


--------------------------------------------------------------------------------
-- | Obtain the music volume
getMusicVolume :: IO Double
getMusicVolume = fmap realToFrac ff_getMusicVolume
{-# INLINE getMusicVolume #-}


--------------------------------------------------------------------------------
-- Play some sound.
--
-- This call is non-blocking.
playSound :: Sound -> IO ()
playSound (Sound fptr) = withForeignPtr fptr ff_playSound


--------------------------------------------------------------------------------
-- | Play a sound with panning
playSoundPanning :: Sound    -- ^ Sound to play
                 -> Double   -- ^ Panning: 0 for completely left, 1 for right
                 -> Double   -- ^ Sound distance: between 0 and 1
                 -> IO ()    -- ^ Non-blocking
playSoundPanning (Sound fptr) panning distance = withForeignPtr fptr $ \ptr ->
    ff_playSoundPanning ptr (realToFrac panning) (realToFrac distance)


--------------------------------------------------------------------------------
-- | Play a sound at a given position defined in radians and distance
--
-- The radians are interpreted like this:
--
-- >    (3 * pi / 4)
-- >
-- > pi    (user)    0
-- >
-- >       pi / 2
--
-- So 0 is directly in front of the user.
playSoundPosition :: Sound   -- ^ Sound to play
                  -> Double  -- ^ Angle in radians
                  -> Double  -- ^ Sound distance: between 0 and 1
                  -> IO ()   -- ^ Non-blocking
playSoundPosition (Sound fptr) angle distance = withForeignPtr fptr $ \ptr ->
    ff_playSoundPosition ptr (realToFrac angle) (realToFrac distance)
