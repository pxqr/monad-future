{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Data.Future.Event
       ( Future(..)
       , Event (..)
       , waitForM
       ) where

import Control.Monad.Reader (MonadIO, liftIO)

-- | Events which we should wait for.
--
--   Future should satify the following laws:
--
--     * idempotence:   waitFor a >> waitFor a = waitFor a
--
--     * commutativity: waitFor a >> waitFor b = waitFor a << waitFor b
--
--     * associativity: (waitFor a >> waitFor b) >> waitFor c
--                        = waitFor a >> (waitFor b >> waitFor c)
--         or maybe associativity can be entailed from monad props...?
--
class Future e where
  waitFor :: e -> IO ()
-- or maybe class Future e m | m -> e ?
-- how define maybe and [] though

-- | Events which might be happened yet.
--
--   /Event/ should satify one law:
--
--     * annihilation: waitFor noWait = return ()
--
--  Properties /derived/ from Future laws:
--
--     * left identity:  waitFor noWait >> waitFor a = waitFor a
--
--     * right identity: waitFor a >> waitFor noWait = waitFor a
--
--
class Future e => Event e where
  noWait  :: e

-- | Lifted version of 'waitFor'.
waitForM :: (MonadIO m, Event e) => e -> m ()
waitForM = liftIO . waitFor


{-# RULES
   "Future/waitFor/idempotence" forall a. waitFor a >> waitFor a = waitFor a;
   "Event/left identity"        forall a. waitFor noWait >> waitFor a = waitFor a;
   "Event/right identity"       forall a. waitFor noWait >> waitFor a = waitFor a
  #-}


instance Future e => Future (Maybe e) where
  waitFor Nothing  = return ()
  waitFor (Just e) = waitFor e
  {-# INLINE waitFor #-}

instance Future e => Event (Maybe e) where
  noWait = Nothing
  {-# INLINE noWait #-}

instance Future e => Future [e] where
  waitFor = mapM_ waitFor
  {-# INLINE waitFor #-}

instance Future e => Event [e] where
  noWait = []
  {-# INLINE noWait #-}
