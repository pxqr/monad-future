module Data.Future.Result
       ( AsyncR (..)
       , SyncR  (..)
       , unsafeAsyncResult
       ) where

import Data.Future.Event
import Data.Monoid


newtype SyncR a = SyncR {
    getSyncR :: a
  } deriving (Show, Read, Eq, Ord)

-- | Async result.
--   We maintain following invariant:
--     `waitFor (resultEvent x)' is before any use of result of `(resultValue x)'
--
data AsyncR e a = AsyncR {
    resultEvent :: e
  , resultValue :: a
  }

instance Future e => Future (AsyncR e a) where
  waitFor = waitFor . resultEvent
  {-# INLINE waitFor #-}

-- TODO:
instance (Monoid a, Event e) => Event (AsyncR e a) where
  noWait = AsyncR noWait mempty
  {-# INLINE noWait #-}


unsafeAsyncResult :: AsyncR e a -> a
unsafeAsyncResult = resultValue
{-# INLINE unsafeAsyncResult  #-}
