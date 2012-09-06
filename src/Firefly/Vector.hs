--------------------------------------------------------------------------------
-- | Small module for vector maths. A Vector usually describes a point in the 2D
-- space, or a difference between two points.
module Firefly.Vector
    ( Vector (..)
    , (.+.)
    , (.-.)
    , (./)
    , neg

    , len

    , fromPolar
    , toPolar
    , fromInts
    , toInts
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid (Monoid (..))


--------------------------------------------------------------------------------
data Vector = Vector
    { vectorX :: {-# UNPACK #-} !Double
    , vectorY :: {-# UNPACK #-} !Double
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Vector where
    mempty  = Vector 0 0
    mappend = (.+.)


--------------------------------------------------------------------------------
(.+.) :: Vector -> Vector -> Vector
(Vector x1 y1) .+. (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
infixl 6 .+.


--------------------------------------------------------------------------------
(.-.) :: Vector -> Vector -> Vector
(Vector x1 y1) .-. (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)
infixl 6 .-.


--------------------------------------------------------------------------------
(./) :: Vector -> Double -> Vector
(Vector x y) ./ s = Vector (x / s) (y / s)
infixl 7 ./


--------------------------------------------------------------------------------
neg :: Vector -> Vector
neg (Vector x y) = Vector (-x) (-y)


--------------------------------------------------------------------------------
len :: Vector -> Double
len (Vector x y) = sqrt $ x * x + y * y


--------------------------------------------------------------------------------
fromPolar :: Double -> Double -> Vector
fromPolar r th = Vector (r * cos th) (r * sin th)


--------------------------------------------------------------------------------
toPolar :: Vector -> (Double, Double)
toPolar v@(Vector x y) = (len v, atan2 y x)


--------------------------------------------------------------------------------
fromInts :: (Int, Int) -> Vector
fromInts (x, y) = Vector (fromIntegral x) (fromIntegral y)


--------------------------------------------------------------------------------
toInts :: Vector -> (Int, Int)
toInts (Vector x y) = (round x, round y)
