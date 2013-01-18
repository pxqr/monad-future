{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Future.Class
       ( MonadFuture (..)
       ) where

import Control.Monad.Writer

-- | Async monad gives the safe and straightforward way to work
--   asynchonous resources.
--
class Monad m => MonadFuture r m | m -> r where
  async :: m a -> m (r a)
  await :: r a -> m a

--    "async" forall a b. async a >> b = b
{-# RULES
    "await" forall a b. await a >> b = b;

  #-}

-- TODO: Instances
--instance (Monoid w, MonadFuture e m) => MonadFuture e (WriterT w m) where
--  await = lift . await
