{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.MonadPlus (
  IsNullM(..)
) where

import Control.Monad

class IsNullM m a where
  isNullM :: m a -> Bool

instance (Monad m, MonadPlus m, Eq (m a)) => IsNullM m a where
  isNullM mv = mv == mzero
