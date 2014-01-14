{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Internal where

import qualified Data.Foldable as F

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

instance IsNull Bool where
  isNull = id
