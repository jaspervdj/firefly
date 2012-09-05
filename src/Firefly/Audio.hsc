--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Audio
    ( Loop (..)
    , playMusic
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types
import           Foreign.C.String


--------------------------------------------------------------------------------
#include "firefly/audio.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_playMusic" ff_playMusic
    :: CString -> CInt -> IO ()


--------------------------------------------------------------------------------
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
playMusic :: FilePath -> Loop -> IO ()
playMusic filePath l = withCString filePath $ \str ->
    ff_playMusic str (fromLoop l)
