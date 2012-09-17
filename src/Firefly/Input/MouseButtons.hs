--------------------------------------------------------------------------------
module Firefly.Input.MouseButtons
    ( left
    , middle
    , right
    ) where


--------------------------------------------------------------------------------
import           Firefly.Input.Internal


--------------------------------------------------------------------------------
left :: MouseButton
left = MouseButton 1
{-# INLINE left #-}


--------------------------------------------------------------------------------
middle :: MouseButton
middle = MouseButton 2
{-# INLINE middle #-}


--------------------------------------------------------------------------------
right :: MouseButton
right = MouseButton 3
{-# INLINE right #-}
