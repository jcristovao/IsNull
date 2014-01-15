{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Internal where

import qualified Data.MonoTraversable as O
import qualified Data.Foldable        as F
import Data.Set as Set

type family Nullable ent
type instance Nullable (Set.Set a) = a
type instance Nullable Bool = Bool

class IsNull a where
  isNull :: a -> Bool

  -- | typing causes arthritis
  isN :: a -> Bool
  isN = isNull

  -- | the logical negation of @'isNull'@
  notNull :: a -> Bool
  notNull = not . isNull

instance (O.MonoFoldable f) => IsNull f where
  isNull = O.onull

instance IsNull Bool where
  isNull = id

instance IsNull (Set.Set a) where
  isNull = Set.null
--
-- | Nested is Null: is the container null, or are all of its items null?
{-isNNull-}
  {-:: (F.Foldable f)-}
  {-=> f g -> Bool-}
{-isNNull = F.all isNull-}
