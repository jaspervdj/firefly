--------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}
module Firefly.Engine
    ( firefly
    , getTicks
    , delay
    ) where


--------------------------------------------------------------------------------
import           Foreign.C.Types
import           Prelude         hiding (init)


--------------------------------------------------------------------------------
foreign import ccall unsafe "ff_init" ff_init  :: IO ()
foreign import ccall unsafe "ff_quit" ff_quit  :: IO ()
foreign import ccall unsafe "ff_getTicks" ff_getTicks :: IO CInt
foreign import ccall unsafe "ff_delay"ff_delay :: CInt -> IO ()


--------------------------------------------------------------------------------
-- | Initialize the engine, execute the given block and quit the engine. You
-- probably want to use this in the main function, e.g.:
--
-- > main :: IO ()
-- > main = firefly $ do
-- >     ...
--
firefly :: IO () -> IO ()
firefly block = do
    ff_init
    block
    ff_quit


--------------------------------------------------------------------------------
-- | Get the number of ticks (milliseconds) that have passed since the
-- initialization of the engine.
getTicks :: IO Int
getTicks = fmap fromIntegral $ ff_getTicks
{-# INLINE getTicks #-}


--------------------------------------------------------------------------------
-- | Sleep for the given amount of milliseconds.
delay :: Int -> IO ()
delay = ff_delay . fromIntegral
{-# INLINE delay #-}
