{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Control.Monad.Future.Async
       ( AsyncT, Async
       , future, future_
       ,  runAsyncT,  runAsync
       , execAsyncT, execAsync
       , evalAsyncT, evalAsync
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader (MonadIO)
import Control.Monad.Trans

import Control.Monad.Future.Class

import Data.Future.Event
import Data.Future.Result


-- | We maintain the following invariant:
--     `waitFor (asyncEvent x)' is before any use of result of `(asyncAction x)'
--
--  Exsamples:
--   clExecKernel :: CLMem -> CLEvent -> IO CLEvent
--   This function wait for events thene _async_ exec  program _immedeately_
--   returning the event. But we don't want to do synchonization explicitly.
--
--   execKernel :: AsyncR CLEvent CLMem -> Async ()
--   execKernel (AsyncR e mem) = future_ $ clExecKernel mem e
--
--   and now if want run execKernel sync we just write
--
--     _ <- execKernel mem
--
--   if we want run execKernel async we write
--
--     r <- async $ execKernel mem
--     ..do something..
--     await r
--
--   or we can even write something like this:
--
--     execKernel mem
--
--   but in this case we can't synchonize no more. Last exsample may useful for
--   something like "write in another thread in socket forever"
--
--   As rule of thumb: if you see MonadIO in signature then there are _maybe_
--   will be synchonization, if not then there are surely no.
--
data AsyncT e m a = AsyncT {
    asyncEvent  :: e
  , asyncAction :: m a
  }

type Async e a = AsyncT e IO a


runAsyncT :: Monad m => AsyncT e m a -> m (e, a)
runAsyncT (AsyncT e m) = m >>= \a -> return (e, a)
{-# INLINE runAsyncT #-}

evalAsyncT :: Monad m => AsyncT e m a -> m a
evalAsyncT = asyncAction
{-# INLINE evalAsyncT #-}

execAsyncT :: Monad m => AsyncT e m a -> m e
execAsyncT (AsyncT e m) = m >>= \_ -> return e
{-# INLINE execAsyncT #-}

runAsync :: Async e a -> IO (e, a)
runAsync = runAsyncT
{-# INLINE runAsync #-}

evalAsync :: Async e a -> IO a
evalAsync = evalAsyncT
{-# INLINE evalAsync #-}

execAsync :: Async e a -> IO e
execAsync = execAsyncT
{-# INLINE execAsync #-}


-- We surely need synchonization here. Exsample:
--   fmap (\io -> io >>= \x ->
--           peekPtr blah blah x)
--
-- prove: fmap id = id -- heh, need we?
--
-- fmap id a = Async noWait (waitFor (asyncEvent a) >> fmap id (asyncAction a))
-- id a = Async noWait (waitFor (asyncEvent a) >> id (asyncAction a))
-- id a = Async noWait (waitFor (asyncEvent a) >> asyncAction a)
-- since we can not use result of (asyncAction a) this is true.
--
-- TODO: prove: fmap f . fmap g = fmap (f . g)
--
instance (Functor m, MonadIO m, Event e) => Functor (AsyncT e m) where
  fmap f a = AsyncT noWait $ do
    waitForM (asyncEvent a)
    fmap f (asyncAction a)

instance (Applicative m, MonadIO m, Event e) => Applicative (AsyncT e m) where
  pure = AsyncT noWait . pure
  {-# INLINE pure #-}

  -- TODO: more effective
  (<*>) = ap


instance (MonadIO m, Event e) => Monad (AsyncT e m) where
  return  = AsyncT noWait . return
  {-# INLINE return #-}

  m >>= f = future $ do
    x <- asyncAction m
    waitForM (asyncEvent m)
    runAsyncT (f x)

  AsyncT _ m >> AsyncT e' m' = AsyncT e' (m >> m')
  {-# INLINE (>>) #-}

instance (MonadIO m, Event e) => MonadFuture (AsyncR e) (AsyncT e m) where
  async (AsyncT e a) = AsyncT noWait (a >>= return . AsyncR e)
  await (AsyncR e a) = AsyncT e (return a)

instance Event e => MonadTrans (AsyncT e) where
  lift = AsyncT noWait
  {-# INLINE lift #-}


-- | Resource will be accessible only in future.
--   In contrast with lift -- resource should be synchonized.
--   Normally this function should be used in only libs and
--   but not in lib user code.
--
future :: (MonadIO m, Event e) => m (e, a) -> AsyncT e m a
future m = AsyncT noWait m >>= \(e, a) -> AsyncT e (return a)
{-# INLINE future #-}

future_ :: (MonadIO m, Event e) => m e -> AsyncT e m ()
future_ m = AsyncT noWait m >>= \e -> AsyncT e (return ())
{-# INLINE future_ #-}


--Async :: Event e => Async e a -> IO a
--runAsync (Async e a) = waitFor e >> a

--execAsync :: Event e => Async e a -> IO
