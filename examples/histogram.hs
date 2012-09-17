--------------------------------------------------------------------------------
import           Control.Monad      (forM, forM_)
import Text.Printf (printf)
import Control.Applicative ((<$>))
import           Data.List          (foldl')
import qualified Data.Map           as M
import qualified Firefly            as F
import           Foreign.Storable   (peek, peekElemOff)
import           System.Environment (getArgs, getProgName)


--------------------------------------------------------------------------------
histogram :: FilePath -> IO ()
histogram filePath = do
    image   <- F.imageFromPng filePath
    colors' <- colors image

    componentHistogram "Red"   (map F.colorR colors')
    componentHistogram "Green" (map F.colorG colors')
    componentHistogram "Blue"  (map F.colorB colors')
    componentHistogram "Alpha" (map F.colorA colors')


--------------------------------------------------------------------------------
componentHistogram :: String -> [Double] -> IO ()
componentHistogram name xs = do
    let steps   = 10 :: Double
        freqs   = frequencies $ map (round . (* steps)) xs :: [(Int, Int)]
        maxFreq = maximum $ map snd freqs

    putStrLn name
    forM_ freqs $ \(x, freq) -> printf "%3f: %s\n"
        (fromIntegral x / steps)
        (replicate (freq * 70 `div` maxFreq) '#')
    putStrLn ""


--------------------------------------------------------------------------------
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = M.toAscList . foldl' (\m x -> M.insertWith' (+) x 1 m) M.empty


--------------------------------------------------------------------------------
colors :: F.Image -> IO [F.Color]
colors image = F.imageWithPixels image $ \pixels ->
    forM [0 .. w * h - 1] $ \i -> do
        bytes <- forM [0 .. bpp - 1] $ \b -> peekElemOff pixels (i * bpp + b)
        let color = makeColor bpp $ scale bytes
        color `seq` return color
  where
    (w, h) = F.imageSize image
    bpp    = F.imageBpp image

    scale  = map ((/ 0xff) . fromIntegral)

    makeColor 3 [r, g, b]    = F.fromRgb r g b
    makeColor 4 [r, g, b, a] = F.fromRgba r g b a
    makeColor b _            = error $ "Invalid bpp: " ++ show b


--------------------------------------------------------------------------------
main :: IO ()
main = do
    args     <- getArgs
    progName <- getProgName
    case args of
        [filePath] -> histogram filePath
        _          -> putStrLn $ "Usage: " ++ progName ++ " <png image>"
