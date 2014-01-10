{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsEmpty.Internal where

import qualified Data.Foldable as F
import Data.Set as Set

-- shamelessly copied from GHC 7.7 Base
#if __GLASGOW_HASKELL__ < 707
instance F.Foldable (Either a) where
    {-foldMap _ (Left _) = mempty-}
    {-foldMap f (Right y) = f y-}

    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z
#endif

class IsEmpty a where
  isEmpty :: a -> Bool

instance (F.Foldable f) => IsEmpty (f a) where
  isEmpty = F.foldr (\_ _ -> False) True

instance IsEmpty (Set.Set a) where
  isEmpty = Set.null

{-instance (IsString s, Eq s) => IsEmpty s where-}
  {-isEmpty s = s == fromString ""-}
