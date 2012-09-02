--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)


--------------------------------------------------------------------------------
import qualified Firefly            as F
import qualified Firefly.Input.Keys as FK
import           Firefly.Vector


--------------------------------------------------------------------------------
main :: IO ()
main = F.firefly $ do
    F.setVideoMode (800, 600)

    img <- F.imageFromPng "example/acid.png"
    putStrLn $ "Image size: " ++ show (F.imageSize img)

    loop img


--------------------------------------------------------------------------------
loop :: F.Image -> IO ()
loop img = do
    F.flushInput
    quit     <- F.receivedQuit
    esc      <- F.keyDown FK.escape
    mousePos <- F.mousePosition
    ticks    <- F.ticks

    F.frame $ do
        screenSize <- F.screenSize
        F.translate $ F.fromInts screenSize ./ 2
        F.rotate $ 2 * pi * fromIntegral ticks / 1000
        F.drawImageCentered img
        -- F.drawImageDebug img

    F.delay 10

    if quit || esc then return () else loop img
