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


