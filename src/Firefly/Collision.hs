--------------------------------------------------------------------------------
module Firefly.Collision
    ( CollisionShape (..)
    , drawCollisionShape
    ) where


--------------------------------------------------------------------------------
import           Firefly.Vector
import           Firefly.Video


--------------------------------------------------------------------------------
data CollisionShape
    -- | Axis-aligned bounding box defined by its center and size
    = CollisionBox Vector Vector


--------------------------------------------------------------------------------
drawCollisionShape :: CollisionShape -> IO ()
drawCollisionShape (CollisionBox center size) = pushMatrix $ do
    translate $ center .-. size ./ 2
    drawLine [Vector 0 0, Vector 0 h, Vector w h, Vector w 0, Vector 0 0]
  where
    Vector w h = size
