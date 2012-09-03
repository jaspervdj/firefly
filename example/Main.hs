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
    font <- F.fontFromTtf "example/japanese.ttf" 80
    putStrLn $ "Image size: " ++ show (F.imageSize img)

    loop img font


--------------------------------------------------------------------------------
loop :: F.Image -> F.Font -> IO ()
loop img font = do
    F.flushInput
    quit       <- F.receivedQuit
    esc        <- F.keyDown FK.escape
    mousePos   <- F.mousePosition
    ticks      <- F.ticks
    screenSize <- F.screenSize

    F.frame $ do
        F.pushMatrix $ do
            let (sw, sh) = screenSize
            F.translate $ F.fromInts (- (ticks `div` 3 `mod` sw), sh - 40)
            F.drawString font "アシッドハウス"
            F.translate $ F.fromInts (sw, 0)
            F.drawString font "アシッドハウス"

        F.translate $ F.fromInts screenSize ./ 2
        F.rotate $ 2 * pi * fromIntegral ticks / 1000
        F.drawImageCentered img
        
        -- F.drawImageDebug img

    F.delay 10

    if quit || esc then return () else loop img font
