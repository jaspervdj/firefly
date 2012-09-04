--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Firefly.Input.MouseButtons
    ( left
    , middle
    , right
    ) where


--------------------------------------------------------------------------------
--
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
#include <SDL.h>


--------------------------------------------------------------------------------
#{enum MouseButton, MouseButton, left = SDL_BUTTON_LEFT}
{-# INLINE left #-}
#{enum MouseButton, MouseButton, middle = SDL_BUTTON_MIDDLE}
{-# INLINE middle #-}
#{enum MouseButton, MouseButton, right = SDL_BUTTON_RIGHT}
{-# INLINE right #-}
