{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.MonoTraversable (
    IsNull(..)
  , isNNull
) where

import Data.IsNull.Internal
import qualified Data.Foldable as F
import Data.Set as Set
import Data.MonoTraversable

instance (MonoFoldable f) => IsNull f where
  isNull = onull

instance IsNull (Set.Set a) where
  isNull = Set.null

-- | Nested is Null: is the container null, or are all of its items null?
isNNull
  :: (F.Foldable f, MonoFoldable g)
  => f g -> Bool
isNNull = F.all onull

