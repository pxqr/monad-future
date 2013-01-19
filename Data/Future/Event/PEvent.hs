module Data.Future.Event.PEvent where

import Control.Monad
import Data.Future.Event
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe

newtype PEvent = PEvent { getFEvent :: ForeignPtr Bool }

instance Future PEvent where
  waitFor (PEvent fptr) = withForeignPtr fptr $ \ptr ->
    let wait = peek ptr >>= (`unless` wait) in wait

instance Event PEvent where
  noWait = unsafePerformIO $ do
    e <- event
    ring e
    return e

event :: IO PEvent
event = do
  fptr <- mallocForeignPtr
  withForeignPtr fptr (`poke` False)
  return $ PEvent fptr

ring :: PEvent -> IO ()
ring e = withForeignPtr (getFEvent e) (`poke` True)