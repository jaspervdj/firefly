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
    putStrLn $ "Available fullscreen modes: " ++ show modes

    F.setVideoMode (800, 600) True

    background  <- F.textureFromPng "examples/background.png"
    font        <- F.fontFromTtf "examples/japanese.ttf" 120
    sound       <- F.soundFromFile "examples/sound.wav"


    tmp <- F.imageFromPng "examples/acid.png"
    img <- F.textureFromImage $ F.imageSlice (100, 100) (200, 200) tmp


    F.playSound sound

    putStrLn $ "Image size: " ++ show (F.textureSize img)
    putStrLn $ "Sound: " ++ F.soundFilePath sound
    F.playMusic "examples/music.mp3" F.DontLoop

    loop background img font


--------------------------------------------------------------------------------
loop :: F.Texture -> F.Texture -> F.Font -> IO ()
loop background img font = do
    F.flushInput
    quit       <- F.hasReceivedQuit
    esc        <- F.isKeyDown FK.escape
    mousePos   <- F.getMousePosition
    ticks      <- F.getTicks
    screenSize <- F.getScreenSize

    let ticks' = fromIntegral (ticks `mod` 1000) / 1000

    F.frame $ do
        let (sw, sh) = screenSize

        F.drawTexture (F.XY (- ticks' * fromIntegral sw) 0) background
        F.drawTexture (F.XY (fromIntegral sw) 0) background

        F.pushColor $ do
            let pos = F.fromInts (sw `div` 2, 0)
            F.setColor $ F.fromRgba 0 0 0 0.2
            F.drawStringCentered pos font "ラーメン"
            F.setColor $ F.fromHsv ticks' 0.8 1
            F.drawStringCentered (pos .-. F.XY 0 (-5)) font "ラーメン"


        F.translate $ XY 0 80 .+. F.fromInts screenSize ./ 2
        F.rotate $ 2 * pi * ticks'
        F.drawTextureCentered (F.XY 0 0) img

        -- F.drawTextureDebug img

    F.delay 10

    if quit || esc then return () else loop background img font
