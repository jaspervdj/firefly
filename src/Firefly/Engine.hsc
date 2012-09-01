--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Engine
    ( init
    , quit
    ) where


--------------------------------------------------------------------------------
import           Prelude hiding (init)


--------------------------------------------------------------------------------
#include "engine.h"


--------------------------------------------------------------------------------
foreign import ccall unsafe "engine_init" engine_init :: IO ()
foreign import ccall unsafe "engine_quit" engine_quit :: IO ()


--------------------------------------------------------------------------------
init :: IO ()
init = engine_init

--------------------------------------------------------------------------------
quit :: IO ()
quit = engine_quit
