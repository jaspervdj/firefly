--------------------------------------------------------------------------------
module Firefly.Video.Color
    ( Color (..)
    , fromRgb
    , fromRgba
    , fromHsv

    , white
    , black
    ) where


--------------------------------------------------------------------------------
data Color = Color
    { colorR :: {-# UNPACK #-} !Double
    , colorG :: {-# UNPACK #-} !Double
    , colorB :: {-# UNPACK #-} !Double
    , colorA :: {-# UNPACK #-} !Double
    } deriving (Show)


--------------------------------------------------------------------------------
fromRgb :: Double -> Double -> Double -> Color
fromRgb r g b = Color r g b 1
{-# INLINE fromRgb #-}


--------------------------------------------------------------------------------
fromRgba :: Double -> Double -> Double -> Double -> Color
fromRgba = Color
{-# INLINE fromRgba #-}


--------------------------------------------------------------------------------
fromHsv :: Double -> Double -> Double -> Color
fromHsv h s v
    | s <= 0    = fromRgb v v v
    | otherwise = case i of
        0 -> fromRgb v t p
        1 -> fromRgb q v p
        2 -> fromRgb p v t
        3 -> fromRgb p q v
        4 -> fromRgb t p v
        _ -> fromRgb v p q
  where
    h'  = h * 6
    i   = floor h' :: Int
    f   = h' - fromIntegral i

    p = v * (1 - s)
    q = v * (1 - s * f)
    t = v * (1 - s * (1 - f))
{-# INLINE fromHsv #-}


--------------------------------------------------------------------------------
white :: Color
white = fromRgb 1 1 1
{-# INLINE white #-}


--------------------------------------------------------------------------------
black :: Color
black = fromRgb 0 0 0
{-# INLINE black #-}
