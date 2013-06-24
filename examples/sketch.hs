--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)


--------------------------------------------------------------------------------
import qualified Firefly                    as F
import qualified Firefly.Input.Keys         as FK
import qualified Firefly.Input.MouseButtons as FMB
import           Firefly.Math.XY


--------------------------------------------------------------------------------
main :: IO ()
main = F.firefly $ do
    modes <- F.getFullScreenModes
    F.setVideoMode (800, 600) False
    F.setBackgroundColor F.white
    F.setSmoothLines True
    F.setLineWidth 1.5
    loop


--------------------------------------------------------------------------------
loop :: IO ()
loop = do
    F.flushInput
    quit <- F.hasReceivedQuit
    esc  <- F.isKeyDown FK.escape

    F.frame $ do
        F.setColor F.black
        F.drawLines
            [ (F.XY 100 100)
            , (F.XY 100 200)
            , (F.XY 200 200)
            , (F.XY 400 100)
            ]

    F.delay 10
    if quit || esc then return () else loop
