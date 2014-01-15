{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | I'm sorry, Dave. I'm afraid I can't do that.
--
-- "Data.Monoid" @'mempty'@ is /not/ a synonym for empty or null.
-- It just means that it is the identity of the @'Monoid'@ binary operation.
-- As a counter example, @getProduct mempty == 1@. Does that seem \'null\' to you?
--
-- Nevertheless, many older or not so maintained modules indeed have a monoid instance
-- meaning \'empty\', and lack a proper @'Foldable'@ instance, thus this module
-- can be useful for those cases.
--
-- However, a too general use can lead to /_WRONG_/ results.
--
-- You have been warned!
module Data.IsNull.MonoidId (
    {-IsNull(..)-}
  {-, isNNull-}
) where

{-import Data.IsNull.Internal-}
{-import qualified Data.Foldable as F-}
{-import Data.Set as Set-}
{-import Data.Monoid-}

{-instance (Monoid oid, Eq oid) => IsNull oid where-}
  {-isNull oid = oid == mempty-}

{-instance IsNull (Set.Set a) where-}
  {-isNull = Set.null-}

-- | Nested is Null: is the container null, or are all of its items null?
{-isNNull-}
  {-:: (F.Foldable f, Monoid g, Eq g)-}
  {-=> f g -> Bool-}
{-isNNull = F.all isNull-}
