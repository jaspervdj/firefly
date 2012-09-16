--------------------------------------------------------------------------------
module Firefly.Video.Internal
    ( CTexture
    , Texture (..)

    , CFont
    , Font (..)

    , withUnicode
    ) where


--------------------------------------------------------------------------------
import           Data.Char             (ord)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr


--------------------------------------------------------------------------------
type CTexture = CChar


--------------------------------------------------------------------------------
newtype Texture = Texture (ForeignPtr CTexture)


--------------------------------------------------------------------------------
type CFont = CChar


--------------------------------------------------------------------------------
newtype Font = Font (ForeignPtr CFont)


--------------------------------------------------------------------------------
withUnicode :: String -> (Ptr CULong -> CInt -> IO a) -> IO a
withUnicode string f = withArrayLen (map (fromIntegral . ord) string) $
    \l ptr -> f ptr (fromIntegral l)
{-# INLINE withUnicode #-}
