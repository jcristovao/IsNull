{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module:      Data.IsNull
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

This class is suitable for use with @GeneralizedNewtypeDeriving@,
thanks to the precious help of Ivan Miljenovic.

Bugs, suggestions and comments are most welcomed!

<https://github.com/jcristovao/IsNull>

-}
module Data.IsNull (
      IsNull(..)
    , notNull
    , isNullN
    , notNullN
    , isNullM
    , isNullNM
    , (<\>)
) where

#if !MIN_VERSION_base(4,7,0)
import qualified Data.Foldable.Compat as F
#else
import qualified Data.Foldable        as F
#endif
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntSet          as IS

-- $setup
-- The code examples in this module require GHC's `OverloadedStrings`
-- extension:
--
-- >>> :set -XOverloadedStrings

class IsNull a where
  -- | isNull ~ isEmpty ~ isInvalid?
  --
  -- >>> isNull (Left 5)
  -- True
  --
  -- >>> isNull ("abc" :: T.Text)
  -- False
  --
  -- >>> isNull [""] -- see isNullN
  -- False
  isNull:: a -> Bool

-- | the logical negation of @'isNull'@
notNull :: (IsNull a) => a -> Bool
notNull = not . isNull

-- | Nested isNull
--
-- >>> isNullN (Just "abc")
-- False
--
-- >>> isNullN (Just "")
-- True
--
-- >>> isNullN (Nothing :: Maybe String)
-- True
isNullN :: (IsNull a, F.Foldable f) => f a -> Bool
isNullN = F.all isNull

-- | Nested isNotNull
notNullN :: (IsNull a, F.Foldable f) => f a -> Bool
notNullN = not . isNullN

-- | Monadic isNull
--
-- >>> isNullM [""]
-- [True]
isNullM :: (IsNull a, Functor m) => m a -> m Bool
isNullM = fmap isNull

-- | Monadic Nested isNull
isNullNM :: (IsNull a, Functor m, F.Foldable f) => m (f a) -> m Bool
isNullNM = fmap isNullN

-- | @'Alternative'@'s @'<|>'@ operator does not always operate as choice,
-- at least not in an intuitive way (for example with lists).
-- This one does:
--
-- >>> [2,3] <\> [4,5]
-- [2,3]
--
-- >>> [] <\> [4,5]
-- [4,5]
--
-- >>> [] <\> []
-- []
(<\>) :: (IsNull a) => a -> a -> a
(<\>) a b = if isNull a then b else a
infixl 3 <\>

instance IsNull T.Text where
  isNull = T.null

instance IsNull LT.Text where
  isNull = LT.null

instance IsNull BS.ByteString where
  isNull = BS.null

instance IsNull LBS.ByteString where
  isNull = LBS.null

instance IsNull IS.IntSet where
  isNull = IS.null

instance F.Foldable f => IsNull (f a) where
  isNull = F.foldr (\_ _ -> False) True


