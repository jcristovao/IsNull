{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      IsNull
Description: A typeclass to determine if a given value is null.
Copyright:   João Cristóvão, 2014
License:     BSD3
Maintainer:  jmacristovao@gmail.com

A typeclass to determine if a given value is null.

Strongly inspired by
<https://hackage.haskell.org/package/mono-traversable mono-traversable>
but with a simpler goal: supporting IsNull and nested IsNull operations.

While the @'isNull'@ function is equivalent to @(==) mempty@ for most of
the instances, not all @'Foldable'@s are monoids,
and not all monoids @'mempty'@ means null:

* @'Either'@ is an example of a @Foldable@ which is not a
  @'Monoid'@, but where it makes sense to consider a @'Left'@ as
  an 'Null' value. While this is not strictly true, the @'Left'@
  option does carries a value, we take the more liberal approach:
  Empty ~ Null ~ Invalid Value.
  If you need proper type reasoning, you should not be using this
  package, just regular pattern matching instead.

* @'Product'@ @'Monoid'@ instance is @1@. Hardly qualifies as an
  @Empty@ or @Null@ value. For this reason no default implementation
  is provided for the @'Monoid'@ class. It's up to you to use
  @(==) mempty@ instead.

The main use case for this package are boolean conditions,
namely the @if@ @then@ @else@ construct.

Bugs, suggestions and comments are most welcomed!

<https://github.com/jcristovao/IsNull>

-}
module Data.IsNull (
    IsNull(..)
) where


import Data.IsNull.Class
#if !MIN_VERSION_base(4,7,0)
import qualified Data.Foldable.Compat as F
#else
import qualified Data.Foldable        as F
#endif


instance F.Foldable f => IsNull (f a) where
  isNull = F.foldr (\_ _ -> False) True


