--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)


--------------------------------------------------------------------------------
import qualified Firefly.Engine     as Engine
import qualified Firefly.Input      as Input
import qualified Firefly.Input.Keys as Keys
import           Firefly.Vector
import qualified Firefly.Video      as Video


--------------------------------------------------------------------------------
main :: IO ()
main = do
    Engine.init
    Video.setMode (800, 600)

    loop

    Engine.quit


--------------------------------------------------------------------------------
loop :: IO ()
loop = do
    Input.flush
    quit     <- Input.quit
    esc      <- Input.keyDown Keys.escape
    mousePos <- Input.mousePosition

    Video.frame $ do
        Video.line [Vector 100 100, Vector 500 100, fromInts mousePos]

    threadDelay 1000

    if quit || esc then return () else loop
