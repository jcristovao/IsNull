{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull (
    IsNull(..)
  , isNNull
) where

import Data.IsNull.Internal
import qualified Data.Foldable as F
import Data.Set as Set
import Data.String

instance (IsString s, Eq s) => IsNull s where
  isNull s = s == fromString ""

instance IsNull (Set.Set a) where
  isNull = Set.null

-- | Nested is Null: is the container null, or are all of its items null?
isNNull
  :: (F.Foldable f, IsString g, Eq g)
  => f g -> Bool
isNNull = F.all isNull
