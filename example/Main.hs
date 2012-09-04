--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever)


--------------------------------------------------------------------------------
import qualified Firefly                    as F
import qualified Firefly.Input.Keys         as FK
import qualified Firefly.Input.MouseButtons as FMB
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
    quit       <- F.hasReceivedQuit
    esc        <- F.isKeyDown FK.escape
    mousePos   <- F.getMousePosition
    ticks      <- F.getTicks
    screenSize <- F.getScreenSize

    let ticks' = fromIntegral (ticks `mod` 2000) / 2000

    F.frame $ do
        F.pushMatrix $ F.pushColor $ do
            let (sw, sh) = screenSize
            F.setColor $ F.fromHsv ticks' 0.8 1
            F.translate $
                F.Vector (- ticks' * fromIntegral sw) (fromIntegral sh - 40)
            F.drawString font "ACIIID"
            F.translate $ F.fromInts (sw, 0)
            F.drawString font "ACIIID"

        F.translate $ F.fromInts screenSize ./ 2
        F.rotate $ 2 * pi * ticks'
        F.drawImageCentered img

        -- F.drawImageDebug img

    F.delay 10

    if quit || esc then return () else loop img font
