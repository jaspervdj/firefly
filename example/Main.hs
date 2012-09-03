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

    img  <- F.imageFromPng "example/acid.png"
    font <- F.fontFromTtf "example/FreeSans.ttf" 80
    putStrLn $ "Image size: " ++ show (F.imageSize img)

    loop img font


--------------------------------------------------------------------------------
loop :: F.Image -> F.Font -> IO ()
loop img font = do
    F.flushInput
    quit     <- F.receivedQuit
    esc      <- F.keyDown FK.escape
    mousePos <- F.mousePosition
    ticks    <- F.ticks

    F.frame $ do
        F.translate $ F.fromInts (50, 80)
        F.drawString font "λf.(λx.f (x x)) (λx.f (x x))"
        F.translate $ F.fromInts (-50, -80)

        screenSize <- F.screenSize
        F.translate $ F.fromInts screenSize ./ 2
        F.rotate $ 2 * pi * fromIntegral ticks / 1000
        F.drawImageCentered img
        
        -- F.drawImageDebug img

    F.delay 10

    if quit || esc then return () else loop img font
