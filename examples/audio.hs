--------------------------------------------------------------------------------
-- | An example showing some of the audio capabilities of firefly
module Main where


--------------------------------------------------------------------------------
import           Control.Monad              (liftM2, unless, when)


--------------------------------------------------------------------------------
import qualified Firefly                    as F
import qualified Firefly.Input.Keys         as FK
import qualified Firefly.Input.MouseButtons as FM
import           Firefly.Math.XY            ((.-.), (./))


--------------------------------------------------------------------------------
main :: IO ()
main = F.firefly $ do
    F.setVideoMode (800, 600) False

    font  <- F.fontFromTtf "examples/FreeSans.ttf" 24
    sound <- F.soundFromFile "examples/hello.wav"

    F.setMusicVolume 0.5
    F.playMusic "examples/music.mp3" F.LoopForever

    loop font sound


--------------------------------------------------------------------------------
loop :: F.Font -> F.Sound -> IO ()
loop font sound = do
    F.flushInput
    quit <- F.hasReceivedQuit

    up   <- F.isKeyDown FK.up
    down <- F.isKeyDown FK.down

    volume <- F.getMusicVolume
    when up   $ F.setMusicVolume $ min 1 $ volume + 0.005
    when down $ F.setMusicVolume $ max 0 $ volume - 0.005

    size <- fmap F.fromInts F.getScreenSize
    pos  <- fmap F.fromInts F.getMousePosition
    let center = size ./ 2

    left  <- F.isMouseButtonPressed FM.left
    right <- F.isMouseButtonPressed FM.right

    when left $ F.playSoundPanning sound
        (F.x pos / F.y size) (abs (F.x (pos .-. center) / F.x center))

    when right $ do
        let (r, th) = F.toPolar (pos .-. center)
        F.playSoundPosition sound th (r / F.len center)

    F.frame $ do
        -- Draw vertical axis
        F.drawLine
            [ F.XY (F.x center) 0
            , F.XY (F.x center) (F.y size)
            ]

        -- Draw horizontal axis
        F.drawLine
            [ F.XY 0 (F.y center)
            , F.XY (F.x size) (F.y center)
            ]

        -- Draw info text
        F.pushMatrix $ do
            F.drawString (F.XY 10 40) font "Up/Down keys to adjust music volume"
            F.drawString (F.XY 10 90) font $
                "Current music volume: " ++ show volume
            F.drawString (F.XY 10 120) font $
                "Left click to play a panned sound"
            F.drawString (F.XY 10 150) font $
                "Right click to play a positioned sound"

    F.delay 10
    unless quit $ loop font sound
