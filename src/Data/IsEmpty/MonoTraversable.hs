{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsEmpty.MonoTraversable (
  IsEmpty(..)
) where

import Data.IsEmpty.Internal
import Data.MonoTraversable

instance (MonoFoldable f) => IsEmpty f where
  isEmpty = onull

