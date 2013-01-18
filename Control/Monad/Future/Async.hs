{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Control.Monad.Future.Async
       ( AsyncT, Async
       , future, future_
       ,  runAsyncT,  runAsync
       , execAsyncT, execAsync
       , evalAsyncT, evalAsync
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Future.Class

import Data.Future.Event
import Data.Future.Result
import Data.Monoid

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
--   will be executed sync.
--
--   As rule of thumb: if you see MonadIO in signature then there are _maybe_
--   will be synchonization, if not then there are surely no.
--
-- Exsamples of async:
--
--   aRes <- async m
--   bRes <- async m'
--   ...
--   a <- await aRes
--   b <- await bRes
--
data AsyncT e m a = AsyncT { runAsyncT :: m (e, a) }

-- | 'AsyncT' version with 'IO' on top.
type Async e a = AsyncT e IO a


evalAsyncT :: Functor f => AsyncT e f a -> f a
evalAsyncT = fmap snd . runAsyncT
{-# INLINE evalAsyncT #-}

execAsyncT :: Functor f => AsyncT e f a -> f e
execAsyncT = fmap fst . runAsyncT
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

-- | Resource will be accessible only in future.
--   In contrast with lift -- resource should be synchonized.
--   Normally this function should be used in only libs and
--   but not in lib user code because 'future' is the only
--   place there consistency can be violated.
--
future :: m (e, a) -> AsyncT e m a
future = AsyncT
{-# INLINE future #-}

future_ :: (Functor m, Event e) => m e -> AsyncT e m ()
future_ = AsyncT . fmap (flip (,) ()) -- XTupleSections?
{-# INLINE future_ #-}

-- We surely don't need synchonization here. Exsample:
--   fmap (\io -> io >>= \x ->
--           peekPtr blah blah x)
-- will run only after action event is performed.
--
-- prove: fmap id = id -- heh, need we?
--
-- fmap id (Async e a) = Async (e (fmap id a))
-- (Async e a) = Async (e (fmap id a))
-- (Async e a) = Async (e a)
--
-- prove: fmap f . fmap g = fmap (f . g)
--
-- fmap f (fmap g (AsyncT e a)) = fmap (f . g) (AsyncT e a))
-- fmap f (AsyncT e (fmap g a)) = AsyncT e (fmap (f . g) a)
-- AsyncT e (fmap f. (fmap g a)) = AsyncT e (fmap (f . g) a)
-- AsyncT e (fmap (f . g) a)) = AsyncT e (fmap (f . g) a)
--
instance Functor f => Functor (AsyncT e f) where
  fmap f = AsyncT . fmap (second f) . runAsyncT
  {-# INLINE fmap #-}

instance (Applicative f, Event e, Monoid e) => Applicative (AsyncT e f) where
  pure = AsyncT . fmap ((,) noWait) . pure
  {-# INLINE pure #-}

  m <*> m' = AsyncT (g <$> runAsyncT m <*> runAsyncT m')
    where
      g (e, f) (e', x) = ((e <> e'), f x)
      {-# INLINE g #-}
  {-# INLINE (<*>) #-}

---------------------- A Consistency -------------------------------------------
-- === What is consistency?
-- Here are that AsyncT consider as /consistency/:
--   Expression of type `Async e a' is consistent iff:
--     * either `a' in sync
--     * or async and associated with /consistent/ event.
--   Event is consistent iff `waitFor event >>' _makes_ future use of
--   associated value consistent.
--
-- === How relates action with consistency?
-- return           | lift in async context _pure_ computation which is
--                  | consistent _yet_.
--
-- m >>= \x -> f x  | make sure that `x' is consistent and chain the action
--                    with it. Here we basically have two cases:
--    * `x' in m already in sync => `x' is consistent;
--    * `x' in m not yet in sync so we should wait for => `x' is consistent.
--
-- === How consistency is : consistency propagation.
-- Here we'll try to prove that >>=/return always gives consistent AsyncT.
--   base case: return --- consistent from definition of return. (already in sync)
--   induction: if `m' is consistent and `f' gives a consistent action then
--               (m >>= f) is consistent from definition of (>>=).
--              `f' gives a consistent action if it takes a consistent value.
--              (why? try to prove, but assume it's true for the first time) <TODO:>
--              `f' takes a consistent value because of definition of (>>=).
--
-- === Future note: 'future' as base case.
-- Actually 'future' is the only place which can violate consistency and it
-- happens iff event associated with value is not consistent.
--                                 (see definition of consistent event)
--
-- === Monad laws <TODO>
-- m >>= return = m      | return always gives consistent action + def of (>>=)
-- return a >>= f = f a  | iff `f' gives a consistent value. (see Future Note)
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
--------------------------------------------------------------------------------

-- Return without newtype wrapper.
pureRes :: (Monad m, Event e) => a -> m (e, a)
pureRes = return . (,) noWait
{-# INLINE pureRes #-}

instance (MonadIO m, Event e) => Monad (AsyncT e m) where
  return = AsyncT . pureRes
  {-# INLINE return #-}

  m >>= f = AsyncT $ do
    (e, x) <- runAsyncT m
    waitForM e
    runAsyncT (f x)

  fail = AsyncT . fail
  -- or maybe: fail = AsyncT . fmap ((,) noWait) . fail
  -- will it violate consistency?


instance (MonadPlus m, MonadIO m, Event e) => MonadPlus (AsyncT e m) where
  mzero = AsyncT mzero
  {-# INLINE mzero #-}

  mplus m m' = AsyncT (runAsyncT m `mplus` runAsyncT m')
  {-# INLINE mplus #-}


-- Maybe MonadIO -> Monad? But it requres to remove Monad constraint
-- in MonadFuture
instance (MonadIO m, Event e) => MonadFuture (AsyncR e) (AsyncT e m) where
  async m = AsyncT $ do
    (e, a) <- runAsyncT m
    return (noWait, AsyncR e a)
  {-# INLINE async #-}

  await (AsyncR e a) = AsyncT (return (e, a))
  {-# INLINE await #-}

instance (MonadIO m, Event e) => MonadIO (AsyncT e m) where
  liftIO m = AsyncT (liftIO m >>= pureRes)
  {-# INLINE liftIO #-}

instance Event e => MonadTrans (AsyncT e) where
  lift m = AsyncT (m >>= pureRes)
  {-# INLINE lift #-}
