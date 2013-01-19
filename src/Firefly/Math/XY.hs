--------------------------------------------------------------------------------
-- | Small module for 2D cartesian coordinates
module Firefly.Math.XY
    ( XY (..)
    , (.+.)
    , (.-.)
    , (.*)
    , (./)
    , (.*.)
    , neg

    , len
    , squaredLen
    , distance

    , fromPolar
    , toPolar
    , fromInts
    , toInts
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid (Monoid (..))


--------------------------------------------------------------------------------
-- | Unboxed (X, Y) tuple
data XY = XY
    { x :: {-# UNPACK #-} !Double
    , y :: {-# UNPACK #-} !Double
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid XY where
    mempty  = XY 0 0
    mappend = (.+.)


--------------------------------------------------------------------------------
(.+.) :: XY -> XY -> XY
XY x1 y1 .+. XY x2 y2 = XY (x1 + x2) (y1 + y2)
infixl 6 .+.
{-# INLINE (.+.) #-}


--------------------------------------------------------------------------------
(.-.) :: XY -> XY -> XY
XY x1 y1 .-. XY x2 y2 = XY (x1 - x2) (y1 - y2)
infixl 6 .-.
{-# INLINE (.-.) #-}


--------------------------------------------------------------------------------
(.*) :: XY -> Double -> XY
XY x' y' .* s = XY (x' * s) (y' * s)
infixl 7 .*
{-# INLINE (.*) #-}


--------------------------------------------------------------------------------
(./) :: XY -> Double -> XY
XY x' y' ./ s = XY (x' / s) (y' / s)
infixl 7 ./
{-# INLINE (./) #-}


--------------------------------------------------------------------------------
-- | Dot product
(.*.) :: XY -> XY -> Double
XY x1 y1 .*. XY x2 y2 = x1 * x2 + y1 * y2
infixl 7 .*.
{-# INLINE (.*.) #-}


--------------------------------------------------------------------------------
neg :: XY -> XY
neg (XY x' y') = XY (-x') (-y')
{-# INLINE neg #-}


--------------------------------------------------------------------------------
len :: XY -> Double
len = sqrt . squaredLen
{-# INLINE len #-}


--------------------------------------------------------------------------------
squaredLen :: XY -> Double
squaredLen (XY x' y') = x' * x' + y' * y'
{-# INLINE squaredLen #-}


--------------------------------------------------------------------------------
distance :: XY -> XY -> Double
distance l r = len (l .-. r)
{-# INLINE distance #-}


--------------------------------------------------------------------------------
fromPolar :: Double  -- ^ Angle in radians
          -> Double  -- ^ Radius
          -> XY      -- ^ Resulting cartesian coordinates
fromPolar r th = XY (r * cos th) (r * sin th)
{-# INLINE fromPolar #-}


--------------------------------------------------------------------------------
toPolar :: XY                -- ^ Input coordinates
        -> (Double, Double)  -- ^ (radius, radians)
toPolar v@(XY x' y') = (len v, atan2 y' x')
{-# INLINE toPolar #-}


--------------------------------------------------------------------------------
fromInts :: (Int, Int) -> XY
fromInts (x', y') = XY (fromIntegral x') (fromIntegral y')
{-# INLINE fromInts #-}


--------------------------------------------------------------------------------
toInts :: XY -> (Int, Int)
toInts (XY x' y') = (round x', round y')
{-# INLINE toInts #-}
