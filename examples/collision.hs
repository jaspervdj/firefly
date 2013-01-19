--------------------------------------------------------------------------------
module Main where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (forM_)
import           System.Random       (randomRIO)


--------------------------------------------------------------------------------
import qualified Firefly             as F
import qualified Firefly.Input.Keys  as FK
import           Firefly.Math.XY


--------------------------------------------------------------------------------
main :: IO ()
main = F.firefly $ do
    F.setVideoMode (800, 600) False
    F.setBackgroundColor $ F.fromRgb 0 0.57 0.70

    font <- F.fontFromTtf "examples/FreeSans.ttf" 14
    xshape <- randomShape
    yshape <- randomShape

    loop xshape yshape font


--------------------------------------------------------------------------------
randomShape :: IO F.Shape
randomShape = do
    shape <- randomRIO (0, 3) :: IO Int
    case shape of
        0 -> do
            p <- randomXY (100, 200)
            s <- randomXY (0, 200)
            return $ F.BoxShape (F.Box p s)
        1 -> do
            p1 <- randomXY (100, 400)
            p2 <- randomXY (100, 400)
            return $ F.LineShape p1 p2
        2 -> do
            p1 <- randomXY (100, 400)
            p2 <- randomXY (100, 400)
            p3 <- randomXY (100, 400)
            return $ F.TriangleShape p1 p2 p3
        _ -> do
            p <- randomXY (100, 400)
            r <- randomRIO (10, 100)
            return $ F.CircleShape p r
  where
    randomXY (lo, up) = XY <$> randomRIO (lo, up) <*> randomRIO (lo, up)


--------------------------------------------------------------------------------
setPos :: XY -> F.Shape -> F.Shape
setPos _ F.EmptyShape               = F.EmptyShape
setPos p (F.AppendShape s k)        =
    F.AppendShape (setPos p s) (setPos p k)
setPos p (F.BoxShape (F.Box _ s))   = F.BoxShape (F.Box p s)
setPos p (F.LineShape p1 p2)        = F.LineShape p (p .+. p1 .-. p2)
setPos p (F.TriangleShape p1 p2 p3) =
    F.TriangleShape p (p .+. p2 .-. p1) (p .+. p3 .-. p1)
setPos p (F.CircleShape _ r)        = F.CircleShape p r


--------------------------------------------------------------------------------
loop :: F.Shape -> F.Shape -> F.Font -> IO ()
loop xshape yshape font = do

    let col = F.collision xshape yshape

    F.frame $ do
        F.setColor $ F.fromRgb 0.58 0.76 0.95
        F.drawShape xshape

        F.setColor $ F.fromRgb 1.00 0.79 0.24
        F.drawShape yshape

        F.setColor $ F.fromRgba 0.71 0.00 0.00 0.70
        forM_ (F.collisionPoints col) $ \p -> F.pushMatrix $ do
            F.translate p
            F.drawCircle 5 10

        F.translate $ F.fromInts (10, 500)
        F.setColor F.white
        F.drawString font $ show col

    F.flushInput
    quit   <- F.hasReceivedQuit
    pos    <- F.getMousePosition
    ticked <- F.isKeyPressed FK.space

    -- Update shapes
    xshape' <- if ticked then randomShape else return xshape
    yshape' <- if ticked then randomShape else return yshape
    let xshape'' = setPos (fromInts pos) xshape'

    F.delay 10
    if quit then return () else loop xshape'' yshape' font
