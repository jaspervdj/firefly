--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Firefly.Math.Shape
    ( Collision (..)
    , collisionPoints
    , Shape (..)
    , collision
    , isCollision
    , shapeBox
    , insideShape
    , moveShape
    , drawShape
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (liftM2)
import           Data.Monoid         (Monoid (..))


--------------------------------------------------------------------------------
import           Firefly.Math.Box
import           Firefly.Math.XY
import           Firefly.Video


--------------------------------------------------------------------------------
data Collision
    = NoCollision     -- ^ No collision happened
    | Collision [XY]  -- ^ Collision, lazily returns intersection points
    deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Collision where
    mempty                                = NoCollision
    mappend NoCollision    y'             = y'
    mappend (Collision xs) NoCollision    = Collision xs
    mappend (Collision xs) (Collision ys) = Collision (xs ++ ys)


--------------------------------------------------------------------------------
collisionPoints :: Collision -> [XY]
collisionPoints NoCollision    = []
collisionPoints (Collision ps) = ps


--------------------------------------------------------------------------------
data Shape
    -- | Empty shape: nothing can collide with this
    = ShapeEmpty
    -- | A composition of two shapes
    | ShapeAppend Shape Shape
    -- | A box
    | ShapeBox Box
    -- | A line connecting two points
    | ShapeLine XY XY
    -- | A triangle
    | ShapeTriangle XY XY XY
    -- | A circle
    | ShapeCircle XY Double
    deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Shape where
    mempty  = ShapeEmpty
    mappend = ShapeAppend


--------------------------------------------------------------------------------
-- | Check for collision between two shapes
collision :: Shape -> Shape -> Collision
-- TODO speed up with bounding boxes?
collision ShapeEmpty _ = NoCollision

collision (ShapeAppend s k) l = collision s l `mappend` collision k l

collision s1@(ShapeBox b1) s2@(ShapeBox b2)
    -- Take advantage of laziness here
    | boxBoxCollision b1 b2 = Collision [] `mappend` slowCollision s1 s2
    | otherwise             = NoCollision

collision (ShapeLine p1 p2) (ShapeLine q1 q2) =
    lineLineIntersection p1 p2 q1 q2

collision (ShapeCircle c1 r1) (ShapeCircle c2 r2) =
    circleCircleCollision c1 r1 c2 r2

collision (ShapeCircle c1 r) s =
    slowCircleCollision c1 r s

collision s (ShapeCircle c1 r) =
    slowCircleCollision c1 r s

collision s k = slowCollision s k


--------------------------------------------------------------------------------
-- | Slow but general implementation for polygon-like stuff
slowCollision :: Shape -> Shape -> Collision
slowCollision s1 s2 = mconcat $
    [Collision [p] | p <- shapePoints s1, p `insideShape` s2] ++
    [Collision [p] | p <- shapePoints s2, p `insideShape` s1] ++
    [ lineLineIntersection p1 p2 q1 q2
    | (p1, p2) <- shapeLines s1
    , (q1, q2) <- shapeLines s2
    ]


--------------------------------------------------------------------------------
slowCircleCollision :: XY -> Double -> Shape -> Collision
slowCircleCollision c r s = mconcat $
    [Collision [p] | p <- shapePoints s, insideCircle p c r] ++
    [Collision [c] | c `insideShape` s] ++
    [lineCircleIntersection p1 p2 c r | (p1, p2) <- shapeLines s]


--------------------------------------------------------------------------------
-- | Check for collision without calculating the intersection points
isCollision :: Shape -> Shape -> Bool
isCollision s1 s2 = case collision s1 s2 of
    NoCollision -> False
    Collision _ -> True


--------------------------------------------------------------------------------
-- | Get a bounding box for the given shape
shapeBox :: Shape -> Maybe Box
shapeBox ShapeEmpty        = Nothing
shapeBox (ShapeAppend s k) = liftM2 appendBox (shapeBox s) (shapeBox k)
shapeBox (ShapeBox b)      = Just b
shapeBox (ShapeCircle p r) =
    let r' = XY r r in Just $ Box (p .-. r') (r' .* 2)
shapeBox s                 = fitBox $ shapePoints s


--------------------------------------------------------------------------------
-- | Check if a point lies in a given shape
insideShape :: XY -> Shape -> Bool
insideShape _ ShapeEmpty = False

insideShape p (ShapeAppend s k) = insideShape p s || insideShape p k

insideShape p (ShapeBox box) = p `insideBox` box

insideShape _ (ShapeLine _ _) = False

insideShape p (ShapeTriangle p1 p2 p3) =
    (u >= 0) && (v >= 0) && (u + v < 1)
  where
    v0 = p2 .-. p1
    v1 = p3 .-. p1
    v2 = p  .-. p1

    dot00 = v0 .*. v0
    dot01 = v0 .*. v1
    dot02 = v0 .*. v2
    dot11 = v1 .*. v1
    dot12 = v1 .*. v2

    invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
    u = (dot11 * dot02 - dot01 * dot12) * invDenom
    v = (dot00 * dot12 - dot01 * dot02) * invDenom

insideShape p (ShapeCircle c r) = insideCircle p c r


--------------------------------------------------------------------------------
-- | Move the shape with an offset
moveShape :: XY -> Shape -> Shape
moveShape _ ShapeEmpty = ShapeEmpty
moveShape o (ShapeAppend s k) =
    ShapeAppend (moveShape o s) (moveShape o k)
moveShape o (ShapeBox (Box pos size)) =
    ShapeBox (Box (pos .+. o) size)
moveShape o (ShapeLine p1 p2) = ShapeLine (p1 .+. o) (p2 .+. o)
moveShape o (ShapeTriangle p1 p2 p3) =
    ShapeTriangle (p1 .+. o) (p2 .+. o) (p3 .+. o)
moveShape o (ShapeCircle p r) = ShapeCircle (p .+. o) r


--------------------------------------------------------------------------------
-- | Draw a shape: this is mostly meant for debugging purposes
drawShape :: Shape -> IO ()
drawShape ShapeEmpty = return ()
drawShape (ShapeAppend s k) =
    drawShape s >> drawShape k
drawShape (ShapeBox (Box pos size)) = pushMatrix $ do
    translate pos
    drawRectangle size
drawShape (ShapeLine v1 v2) = drawLine [v1, v2]
drawShape (ShapeTriangle v1 v2 v3) = drawTriangle v1 v2 v3
drawShape (ShapeCircle p r) = pushMatrix $ do
    translate p
    drawCircle r (floor r)


--------------------------------------------------------------------------------
shapePoints :: Shape -> [XY]
shapePoints ShapeEmpty = []
shapePoints (ShapeAppend s k) = shapePoints s ++ shapePoints k
shapePoints (ShapeBox (Box (XY x' y') (XY w h))) =
    [XY x' y', XY x' (y' + h), XY (x' + w) (y' + h), XY (x' + w) y']
shapePoints (ShapeLine p1 p2) = [p1, p2]
shapePoints (ShapeTriangle p1 p2 p3) = [p1, p2, p3]
shapePoints (ShapeCircle _ _) = []  -- TODO


--------------------------------------------------------------------------------
shapeLines :: Shape -> [(XY, XY)]
shapeLines ShapeEmpty = []
shapeLines (ShapeAppend s k) = shapeLines s ++ shapeLines k
shapeLines (ShapeLine p1 p2) = [(p1, p2)]
shapeLines s =
    -- General implementation for other shapes...
    let ps = shapePoints s
    in zip ps $ drop 1 $ cycle ps


--------------------------------------------------------------------------------
lineLineIntersection :: XY -> XY -> XY -> XY -> Collision
lineLineIntersection p1 p2 q1 q2
    | rs == 0   = NoCollision  -- Parallel
    | t < 0     = NoCollision
    | t > 1     = NoCollision
    | u < 0     = NoCollision
    | u > 1     = NoCollision
    | otherwise = Collision [p1 .+. r .* t]
  where
    !r = p2 .-. p1
    !s = q2 .-. q1

    !rs = r `crossProduct` s
    !qp = q1 .-. p1

    t = qp `crossProduct` (s ./ rs)
    u = qp `crossProduct` (r ./ rs)
{-# INLINE lineLineIntersection #-}


--------------------------------------------------------------------------------
crossProduct :: XY -> XY -> Double
crossProduct (XY x1 y1) (XY x2 y2) = x1 * y2 - y1 * x2
{-# INLINE crossProduct #-}


--------------------------------------------------------------------------------
insideCircle :: XY -> XY -> Double -> Bool
insideCircle p c r = squaredLen (c .-. p) <= r * r
{-# INLINE insideCircle #-}


--------------------------------------------------------------------------------
lineCircleIntersection :: XY -> XY -> XY -> Double -> Collision
lineCircleIntersection p1 p2 c1 r = mconcat
    [ Collision [p1 .+. d .* t]
    | t <- solveQuadratic (d .*. d) (2 * (f .*. d)) (f .*. f - r * r)
    , t >= 0 && t <= 1
    ]
  where
    d = p2 .-. p1
    f = p1 .-. c1
{-# INLINE lineCircleIntersection #-}


--------------------------------------------------------------------------------
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic !a !b !c
    | d < 0     = []
    | d == 0    = [-b / (2 * a)]
    | otherwise = [(-b + sd) / (2 * a), (-b - sd) / (2 * a)]
  where
    d  = b * b - 4 * a * c
    sd = sqrt d
{-# INLINE solveQuadratic #-}


--------------------------------------------------------------------------------
circleCircleCollision :: XY -> Double -> XY -> Double -> Collision
circleCircleCollision c1 r1 c2 r2
    | d > r1 + r2       = NoCollision
    | d < abs (r1 - r2) = Collision insides
    | otherwise         = Collision $
        [XY (px + dy) (py - dx), XY (px - dy) (py + dx)] ++ insides
  where
    delta = c2 .-. c1

    d = len delta
    a = (r1 * r1 - r2 * r2 + d * d) / (2 * d)
    h = sqrt $ r1 * r1 - a * a

    XY px py = c1 .+. delta .* (a / d)
    XY dx dy = delta .* (h / d)

    insides = [c1 | insideCircle c1 c2 r2] ++ [c2 | insideCircle c2 c1 r1]
