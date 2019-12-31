{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor,DeriveTraversable,GeneralizedNewtypeDeriving #-}

module UnsafeError
  (
    UnsafeError, runUnsafeError, mapUnsafeError
  , UnsafeErrorT(..), mapUnsafeErrorT
  ) where

import Data.Functor.Classes
import Data.Functor.Contravariant
import Control.Monad.Identity
import Control.Monad.Fail
import Control.Applicative

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Free

type UnsafeError = UnsafeErrorT Identity

runUnsafeError :: UnsafeError a -> a
runUnsafeError = runIdentity . runUnsafeErrorT
{-# INLINE runUnsafeError #-}

mapUnsafeError :: (a -> b) -> UnsafeError a -> UnsafeError b
mapUnsafeError f = mapUnsafeErrorT (Identity . f . runIdentity)
{-# INLINE mapUnsafeError #-}

newtype UnsafeErrorT m a = UnsafeErrorT { runUnsafeErrorT :: m a }
                         deriving (Eq1,Ord1,Read1,Show1
                                  ,Eq,Ord,Read,Show
                                  ,Functor,Foldable,Traversable,Contravariant
                                  ,Applicative,Alternative,Monad
                                  ,MonadPlus,MonadFix,MonadIO
                                  ,MonadReader r,MonadWriter w,MonadState s)

instance Monad m => MonadFail (UnsafeErrorT m) where fail = error

instance MonadTrans UnsafeErrorT where
  lift = UnsafeErrorT
  {-# INLINE lift #-}

mapUnsafeErrorT :: (m a -> n b) -> UnsafeErrorT m a -> UnsafeErrorT n b
mapUnsafeErrorT f = UnsafeErrorT . f . runUnsafeErrorT
{-# INLINE mapUnsafeErrorT #-}

instance (Monad m,Functor f) => MonadFree f (UnsafeErrorT (FreeT f m))
