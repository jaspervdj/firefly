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

    img <- F.imageCreate

    loop

    F.quit


--------------------------------------------------------------------------------
loop :: IO ()
loop = do
    F.flushInput
    quit     <- F.receivedQuit
    esc      <- F.keyDown FK.escape
    mousePos <- F.mousePosition

    F.frame $ do
        F.line [F.Vector 100 100, F.Vector 500 100, F.fromInts mousePos]

    threadDelay 1000

    if quit || esc then return () else loop
