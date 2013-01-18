{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Control.Monad.Future.Sync where

import Control.Applicative
import Control.Monad.Future.Class
import Control.Monad.Identity

import Data.Future.Result

newtype SyncT m a = SyncT { runSyncT :: m a }
                 deriving (Functor, Applicative, Alternative, Monad)

type Sync = SyncT Identity

runSync :: Sync a -> a
runSync = runIdentity . runSyncT

instance Monad m => MonadFuture SyncR (SyncT m) where
  async = (>>= return . SyncR)
  await = return . getSyncR