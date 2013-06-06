--------------------------------------------------------------------------------
-- | Fast axis-aligned boxes
module Firefly.Math.Box
    ( Box (..)
    , appendBox
    , fitBox
    , insideBox
    , boxBoxCollision
    , moveBox
    ) where


--------------------------------------------------------------------------------
import           Firefly.Math.XY


--------------------------------------------------------------------------------
-- | Axis-aligned bounding box defined by its top-left coordinate and size
data Box = Box
    { boxPosition :: {-# UNPACK #-} !XY
    , boxSize     :: {-# UNPACK #-} !XY
    } deriving (Show)


--------------------------------------------------------------------------------
-- | Create a new box which fits around two boxes
appendBox :: Box -> Box -> Box
appendBox (Box (XY x1 y1) (XY w1 h1)) (Box (XY x2 y2) (XY w2 h2)) =
    Box (XY left top) (XY (right - left) (bottom - top))
  where
    top    = min y1 y2
    right  = max (x1 + w1) (x2 + w2)
    bottom = max (y1 + h1) (y2 + h2)
    left   = min x1 x2
{-# INLINE appendBox #-}


--------------------------------------------------------------------------------
-- | Create a box which fits around all the points in the given list. Returns
-- nothing if an empty list is given.
fitBox :: [XY] -> Maybe Box
fitBox [] = Nothing
fitBox ps = Just $
    Box (XY left top) (XY (right - left) (bottom - top))
  where
    top    = minimum $ map y ps
    right  = maximum $ map x ps
    bottom = maximum $ map y ps
    left   = minimum $ map x ps


--------------------------------------------------------------------------------
-- | Check if a point lies inside a box
insideBox :: XY -> Box -> Bool
insideBox (XY px py) (Box (XY bx by) (XY bw bh)) =
    px >= bx && px < bx + bw && py >= by && py < by + bh
{-# INLINE insideBox #-}


--------------------------------------------------------------------------------
-- | Check if a box intersects with another box
boxBoxCollision :: Box -> Box -> Bool
boxBoxCollision (Box (XY x1 y1) (XY w1 h1)) (Box (XY x2 y2) (XY w2 h2))
    | x1 + w1 < x2 = False
    | x1 > x2 + w2 = False
    | y1 + h1 < y2 = False
    | y1 > y2 + h2 = False
    | otherwise    = True
{-# INLINE boxBoxCollision #-}


--------------------------------------------------------------------------------
-- | Move a box by a given offset
moveBox :: XY -> Box -> Box
moveBox offset (Box pos size) = Box (pos .+. offset) size
{-# INLINE moveBox #-}
