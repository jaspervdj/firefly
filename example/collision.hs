--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)
import           Data.Monoid                (mempty)


--------------------------------------------------------------------------------
import qualified Firefly                    as F
import qualified Firefly.Input.Keys         as FK
import qualified Firefly.Input.MouseButtons as FMB
import           Firefly.Vector


--------------------------------------------------------------------------------
main :: IO ()
main = F.firefly $ do
    F.setVideoMode (800, 600)

    font <- F.fontFromTtf "example/FreeSans.ttf" 12
    let shape = F.CollisionBox (Vector 400 400) (Vector 100 50)

    loop shape font


--------------------------------------------------------------------------------
setPos :: Vector -> F.CollisionShape -> F.CollisionShape
setPos pos (F.CollisionBox _ size) = F.CollisionBox pos size


--------------------------------------------------------------------------------
loop :: F.CollisionShape -> F.Font -> IO ()
loop shape font = do
    F.flushInput
    quit <- F.hasReceivedQuit
    pos  <- F.getMousePosition

    let shape' = setPos (fromInts pos) shape

    F.frame $ do
        F.setColor $ F.fromRgb 1 1 1
        F.drawCollisionShape shape

    F.delay 10
    if quit then return () else loop shape' font
