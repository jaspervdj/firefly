module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import qualified Firefly.Engine as Engine
import qualified Firefly.Video as Video
import Firefly.Vector

main :: IO ()
main = do
    Engine.init
    Video.setMode (800, 600)

    forever $ do
        Video.frame $ do
            Video.line [Vector 100 100, Vector 500 100, Vector 500 500]

        threadDelay $ 1000 * 1000

    Engine.quit
