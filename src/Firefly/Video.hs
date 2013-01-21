--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Video
    ( setVideoMode
    , getScreenSize
    , isFullScreen
    , getFullScreenModes

    , setShowCursor
    , isShowCursor

    , frame

    , drawLine
    , drawTriangle
    , drawQuad
    , drawRectangle
    , drawCircle

    , drawTexture
    , drawTextureCentered
    , drawTextureDebug

    , drawString
    , drawStringCentered

    , pushMatrix
    , translate
    , rotate
    , scale

    , pushColor
    , setColor
    , getColor
    , setBackgroundColor
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>))
import           Control.Monad.Trans    (MonadIO, liftIO)
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable


--------------------------------------------------------------------------------
import           Firefly.Internal
import           Firefly.Math.XY
import           Firefly.Video.Color
import           Firefly.Video.Internal


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_setVideoMode" ff_setVideoMode
    :: CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "ff_getScreenWidth" ff_getScreenWidth :: IO CInt
foreign import ccall unsafe "ff_getScreenHeight" ff_getScreenHeight :: IO CInt
foreign import ccall unsafe "ff_isFullScreen" ff_isFullScreen :: IO CInt
foreign import ccall unsafe "ff_getFullScreenModes" ff_getFullScreenModes
    :: CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ff_setShowCursor" ff_setShowCursor :: CInt -> IO ()
foreign import ccall unsafe "ff_isShowCursor" ff_isShowCursor :: IO CInt
foreign import ccall unsafe "ff_startFrame" ff_startFrame :: IO ()
foreign import ccall unsafe "ff_endFrame" ff_endFrame :: IO ()
foreign import ccall unsafe "ff_startLine" ff_startLine :: IO ()
foreign import ccall unsafe "ff_endLine" ff_endLine :: IO ()
foreign import ccall unsafe "ff_startTriangles" ff_startTriangles :: IO ()
foreign import ccall unsafe "ff_endTriangles" ff_endTriangles :: IO ()
foreign import ccall unsafe "ff_startQuads" ff_startQuads :: IO ()
foreign import ccall unsafe "ff_endQuads" ff_endQuads :: IO ()
foreign import ccall unsafe "ff_vertex" ff_vertex
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_drawCircle" ff_drawCircle
    :: CDouble -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall unsafe "ff_drawTexture" ff_drawTexture
    :: CDouble -> CDouble -> Ptr CTexture -> IO ()
foreign import ccall unsafe "ff_drawTextureCentered" ff_drawTextureCentered
    :: CDouble -> CDouble -> Ptr CTexture -> IO ()
foreign import ccall unsafe "ff_drawTextureDebug" ff_drawTextureDebug
    :: CDouble -> CDouble -> Ptr CTexture -> IO ()
foreign import ccall unsafe "ff_drawString" ff_drawString
    :: CDouble -> CDouble -> Ptr CFont -> Ptr CULong -> CInt -> IO ()
foreign import ccall unsafe "ff_drawStringCentered" ff_drawStringCentered
    :: CDouble -> CDouble -> Ptr CFont -> Ptr CULong -> CInt -> IO ()
foreign import ccall unsafe "ff_pushMatrix" ff_pushMatrix :: IO ()
foreign import ccall unsafe "ff_popMatrix" ff_popMatrix :: IO ()
foreign import ccall unsafe "ff_translate" ff_translate
    :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_rotate" ff_rotate :: CDouble -> IO ()
foreign import ccall unsafe "ff_scale" ff_scale :: CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_setColor" ff_setColor
    :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()
foreign import ccall unsafe "ff_getColor" ff_getColor
    :: Ptr CDouble -> IO ()
foreign import ccall unsafe "ff_setBackgroundColor" ff_setBackgroundColor
    :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()


--------------------------------------------------------------------------------
setVideoMode :: (Int, Int) -> Bool -> IO ()
setVideoMode (width, height) fullScreen = ff_setVideoMode
    (fromIntegral width) (fromIntegral height) (fromBool fullScreen)
{-# INLINE setVideoMode #-}


--------------------------------------------------------------------------------
getScreenSize :: IO (Int, Int)
getScreenSize = do
    w <- ff_getScreenWidth
    h <- ff_getScreenHeight
    return (fromIntegral w, fromIntegral h)
{-# INLINE getScreenSize #-}


--------------------------------------------------------------------------------
isFullScreen :: IO Bool
isFullScreen = toBool <$> ff_isFullScreen
{-# INLINE isFullScreen #-}


--------------------------------------------------------------------------------
getFullScreenModes :: IO [(Int, Int)]
getFullScreenModes =
    allocaArray (maxModes * 32) $ \ptr -> do
        numModes <- ff_getFullScreenModes (fromIntegral maxModes) ptr
        array    <- peekArray (fromIntegral numModes * 2) ptr
        return $ pairs $ map fromIntegral array
  where
    maxModes = 32

    pairs (w : h : zs) = (w, h) : pairs zs
    pairs _            = []


--------------------------------------------------------------------------------
setShowCursor :: Bool -> IO ()
setShowCursor = ff_setShowCursor . fromBool
{-# INLINE setShowCursor #-}


--------------------------------------------------------------------------------
isShowCursor :: IO Bool
isShowCursor = toBool <$> ff_isShowCursor
{-# INLINE isShowCursor #-}


--------------------------------------------------------------------------------
frame :: MonadIO m => m () -> m ()
frame block = do
    liftIO ff_startFrame
    block
    liftIO ff_endFrame
{-# INLINE frame #-}


--------------------------------------------------------------------------------
drawLine :: [XY] -> IO ()
drawLine vectors = do
    ff_startLine
    mapM_ vertex vectors
    ff_endLine
{-# INLINE drawLine #-}


--------------------------------------------------------------------------------
drawTriangle :: XY -> XY -> XY -> IO ()
drawTriangle v1 v2 v3 = do
    ff_startTriangles
    vertex v1 >> vertex v2 >> vertex v3
    ff_endTriangles
{-# INLINE drawTriangle #-}


--------------------------------------------------------------------------------
drawQuad :: XY -> XY -> XY -> XY -> IO ()
drawQuad v1 v2 v3 v4 = do
    ff_startQuads
    vertex v1 >> vertex v2 >> vertex v3 >> vertex v4
    ff_endQuads
{-# INLINE drawQuad #-}


--------------------------------------------------------------------------------
drawRectangle :: XY -> XY -> IO ()
drawRectangle (XY x' y') (XY w h) = drawQuad
    (XY x' y') (XY x' (y' + h)) (XY (x' + w) (y' + h)) (XY (x' + w) y')
{-# INLINE drawRectangle #-}


--------------------------------------------------------------------------------
vertex :: XY -> IO ()
vertex (XY x' y') = ff_vertex (realToFrac x') (realToFrac y')
{-# INLINE vertex #-}


--------------------------------------------------------------------------------
drawCircle :: XY -> Double -> Int -> IO ()
drawCircle (XY x' y') r steps = ff_drawCircle
    (realToFrac x') (realToFrac y') (realToFrac r) (fromIntegral steps)
{-# INLINE drawCircle #-}


--------------------------------------------------------------------------------
drawTexture :: XY -> Texture -> IO ()
drawTexture (XY x' y') (Texture fptr) = withForeignPtr fptr $
    ff_drawTexture (realToFrac x') (realToFrac y')
{-# INLINE drawTexture #-}


--------------------------------------------------------------------------------
drawTextureCentered :: XY -> Texture -> IO ()
drawTextureCentered (XY x' y') (Texture fptr) = withForeignPtr fptr $
    ff_drawTextureCentered (realToFrac x') (realToFrac y')
{-# INLINE drawTextureCentered #-}


--------------------------------------------------------------------------------
drawTextureDebug :: XY -> Texture -> IO ()
drawTextureDebug (XY x' y') (Texture fptr) = withForeignPtr fptr $
    ff_drawTextureDebug (realToFrac x') (realToFrac y')


--------------------------------------------------------------------------------
drawString :: XY -> Font -> String -> IO ()
drawString (XY x' y') (Font fptr) string = withForeignPtr fptr $
    withUnicode string . ff_drawString (realToFrac x') (realToFrac y')
{-# INLINE drawString #-}


--------------------------------------------------------------------------------
drawStringCentered :: XY -> Font -> String -> IO ()
drawStringCentered (XY x' y') (Font fptr) string = withForeignPtr fptr $
    withUnicode string . ff_drawStringCentered (realToFrac x') (realToFrac y')
{-# INLINE drawStringCentered #-}


--------------------------------------------------------------------------------
-- | Pushes the current transformation matrix on the stack, executes the given
-- block of code and then pops the matrix again.
pushMatrix :: MonadIO m => m () -> m ()
pushMatrix block = do
    liftIO ff_pushMatrix
    block
    liftIO ff_popMatrix
{-# INLINE pushMatrix #-}


--------------------------------------------------------------------------------
translate :: XY -> IO ()
translate (XY x' y') = ff_translate (realToFrac x') (realToFrac y')
{-# INLINE translate #-}


--------------------------------------------------------------------------------
-- | Rotate the transformation matrix by the given amount of /radians/
rotate :: Double -> IO ()
rotate r = ff_rotate (realToFrac r)
{-# INLINE rotate #-}


--------------------------------------------------------------------------------
scale :: XY -> IO ()
scale (XY x' y') = ff_scale (realToFrac x') (realToFrac y')
{-# INLINE scale #-}


--------------------------------------------------------------------------------
-- | Pushes the current color on a stack, execute the given block of code and
-- pops the color again.
pushColor :: MonadIO m => m () -> m ()
pushColor block = do
    color <- liftIO getColor
    block
    liftIO $ setColor color
{-# INLINE pushColor #-}


--------------------------------------------------------------------------------
setColor :: Color -> IO ()
setColor (Color r g b a) = ff_setColor
    (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
{-# INLINE setColor #-}


--------------------------------------------------------------------------------
getColor :: IO Color
getColor = allocaArray 4 $ \ptr -> do
    ff_getColor ptr
    r <- realToFrac <$> peek ptr
    g <- realToFrac <$> peekElemOff ptr 1
    b <- realToFrac <$> peekElemOff ptr 2
    a <- realToFrac <$> peekElemOff ptr 3
    return $ Color r g b a
{-# INLINE getColor #-}


--------------------------------------------------------------------------------
setBackgroundColor :: Color -> IO ()
setBackgroundColor (Color r g b a) = ff_setBackgroundColor
    (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
{-# INLINE setBackgroundColor #-}
