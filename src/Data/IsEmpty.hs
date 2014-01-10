{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsEmpty (
  IsEmpty(..)
) where

import Data.IsEmpty.Internal
import Data.String

instance (IsString s, Eq s) => IsEmpty s where
  isEmpty s = s == fromString ""
