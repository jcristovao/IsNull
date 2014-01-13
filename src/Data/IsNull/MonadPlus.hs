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
import qualified Data.Foldable as F
import Data.IsNull
import Data.String

class IsNullM m a where
  isNullM :: m a -> m Bool

-- this one only applies to Text and Bytestring
instance (Monad m, IsString f, Eq f) => IsNullM m f where
  isNullM = liftM isNull

instance (Monad m, F.Foldable f, IsString a, Eq a) => IsNullM m (f a) where
  isNullM = liftM isNNull
