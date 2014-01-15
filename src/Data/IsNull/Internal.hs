{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Internal where

import qualified Data.MonoTraversable as O
import qualified Data.Foldable        as F
import Control.Monad (liftM)
import Data.Set as Set

instance O.MonoFoldable (Either a b)

class IsNull a where
  type Nullable a
  isNull :: Nullable a -> Bool

  -- | Typing causes arthritis
  isN :: Nullable a -> Bool
  isN = isNull

  -- | the logical negation of @'isNull'@
  notNull :: Nullable a -> Bool
  notNull = not . isNull

  -- | Nested isNull
  isNullN :: F.Foldable f => f (Nullable a) -> Bool
  isNullN = F.all isNull

  -- | Nested isNotNull
  notNullN :: F.Foldable f => f (Nullable a) -> Bool
  notNullN = not . isNullN

  -- | Monadic Null
  isNullM :: Monad m => m (Nullable a) -> m Bool
  isNullM = liftM isNull

  -- | Monadic Nested Null
  isNullNM :: (Monad m, F.Foldable f) => m (f (Nullable a)) -> m Bool
  isNullNM = liftM isNullN


instance IsNull Bool where
  type Nullable Bool = Bool
  isNull = id

instance IsNull (Set.Set a) where
  type Nullable (Set.Set a) = Set.Set a
  isNull = Set.null

instance (O.MonoFoldable f) => IsNull f where
  type Nullable f = f
  isNull = O.onull

