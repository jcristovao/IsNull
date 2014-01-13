{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Internal where

import qualified Data.Foldable as F
import Data.Monoid (mempty)

-- shamelessly copied from GHC 7.7 Base
#if __GLASGOW_HASKELL__ < 707
instance F.Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y

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

instance (F.Foldable f) => IsNull (f a) where
  isNull = F.foldr (\_ _ -> False) True



