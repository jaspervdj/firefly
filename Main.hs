--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)


--------------------------------------------------------------------------------
import qualified Firefly            as F
import qualified Firefly.Input.Keys as FK


--------------------------------------------------------------------------------
main :: IO ()
main = do
    F.init
    F.setVideoMode (800, 600)

    img <- F.imageFromNoise (100, 200)

    loop img

    F.quit


--------------------------------------------------------------------------------
loop :: F.Image -> IO ()
loop img = do
    F.flushInput
    quit     <- F.receivedQuit
    esc      <- F.keyDown FK.escape
    mousePos <- F.mousePosition

    F.frame $ do
        F.drawImage img

    threadDelay 1000

    if quit || esc then return () else loop img
