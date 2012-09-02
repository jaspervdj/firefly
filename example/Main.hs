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

    img <- F.imageFromPng "example/acid.png"
    putStrLn $ "Image size: " ++ show (F.imageSize img)

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
        F.drawImageDebug img

    threadDelay 1000

    if quit || esc then return () else loop img
