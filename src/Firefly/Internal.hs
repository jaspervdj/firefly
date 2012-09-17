--------------------------------------------------------------------------------
-- | Some internal utility functions
module Firefly.Internal
    ( fromBool
    , toBool
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types (CInt)


--------------------------------------------------------------------------------
toBool :: CInt -> Bool
toBool = (/= 0)
{-# INLINE toBool #-}


--------------------------------------------------------------------------------
fromBool :: Bool -> CInt
fromBool True  = 1
fromBool False = 0
{-# INLINE fromBool #-}
