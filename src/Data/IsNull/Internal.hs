{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Internal where

import qualified Data.Foldable        as F
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as Set
import Control.Monad (liftM)

class IsNull a where
  type Nullable a
  isNull :: Nullable a -> Bool

  -- | Typing causes arthritis
  isN :: Nullable a -> Bool

  -- | the logical negation of @'isNull'@
  notNull :: Nullable a -> Bool
  {-notNull = not . isNull-}

  -- | Nested isNull
  isNullN :: F.Foldable f => f (Nullable a) -> Bool
  {-isNullN = F.all isNull-}

  -- | Nested isNotNull
  notNullN :: F.Foldable f => f (Nullable a) -> Bool
  {-notNullN = not . isNullN-}

  -- | Monadic Null
  isNullM :: Monad m => m (Nullable a) -> m Bool
  {-isNullM = liftM isNull-}

  -- | Monadic Nested Null
  isNullNM :: (Monad m, F.Foldable f) => m (f (Nullable a)) -> m Bool
  {-isNullNM = liftM isNullN-}


instance IsNull Bool where
  type Nullable Bool = Bool
  isNull = id

instance IsNull T.Text where
  type Nullable T.Text = T.Text
  isNull = T.null

instance IsNull LT.Text where
  type Nullable LT.Text = LT.Text
  isNull = LT.null

instance IsNull BS.ByteString where
  type Nullable BS.ByteString = BS.ByteString
  isNull = BS.null

instance IsNull LBS.ByteString where
  type Nullable LBS.ByteString = LBS.ByteString
  isNull = LBS.null

instance IsNull (Set.Set a) where
  type Nullable (Set.Set a) = Set.Set a
  isNull = Set.null

instance (F.Foldable a)
  => IsNull (a b) where
  type Nullable (a b) = (a b)
  isNull = F.foldr (\_ _ -> False) True

-- catch all error raising
{-instance-}
     {-IsNull a where-}
  {-type Nullable a = a-}
  {-isNull = undefined-}
