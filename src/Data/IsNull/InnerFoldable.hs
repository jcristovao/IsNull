{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.InnerFoldable (
  IsNull(..)
) where

import qualified Data.Foldable as F
import Data.MonoTraversable
import Data.String

-- shamelessly copied from GHC 7.7 Base
#if __GLASGOW_HASKELL__ < 707
instance F.Foldable (Either a) where
    {-foldMap _ (Left _) = mempty-}
    {-foldMap f (Right y) = f y-}

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z
#endif

class IsNull a where
  isNull :: a -> Bool

  -- | typing causes arthritis
  isN :: a -> Bool
  isN = isNull

  -- | the logical negation of @'isNull'@
  notNull :: a -> Bool
  notNull = not . isNull


{-instance (IsString s, Eq s) => IsNull s where-}
  {-isNull s = s == fromString ""-}

-- | This instance may raise problems...
-- This returns True for Just "", and the user may expect it to
-- return False.
instance (F.Foldable f, MonoFoldable g, Eq g) => IsNull (f g) where
  isNull = F.all isNull

instance (MonoFoldable f) => IsNull f where
  isNull = onull
