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
import qualified Data.Set             as Set
import qualified Data.IntSet          as IS
import Control.Monad (liftM)

type family Element mono
type instance Element BS.ByteString  = BS.ByteString
type instance Element LBS.ByteString = LBS.ByteString
type instance Element T.Text         = T.Text
type instance Element LT.Text        = LT.Text
type instance Element [a]            = [a]
type instance Element (Maybe a)      = a
type instance Element (Either b a)   = a

class IsNull a where
  isNull:: a -> Bool

  -- | Typing causes arthritis
  isN :: a -> Bool
  isN = isNull

  -- | the logical negation of @'isNull'@
  notNull :: a -> Bool
  notNull = not . isNull

  -- | Nested isNull
  isNullN :: F.Foldable f => f a -> Bool
  isNullN = F.all isNull

  -- | Nested isNotNull
  notNullN :: F.Foldable f => f a -> Bool
  notNullN = not . isNullN

  -- | Monadic Null
  isNullM :: Monad m => m a -> m Bool
  isNullM = liftM isNull

  -- | Monadic Nested Null
  isNullNM :: (Monad m, F.Foldable f) => m (f a) -> m Bool
  isNullNM = liftM isNullN


instance IsNull Bool where
  isNull = id

instance IsNull T.Text where
  isNull = T.null

instance IsNull LT.Text where
  isNull = LT.null

instance IsNull BS.ByteString where
  isNull = BS.null

instance IsNull LBS.ByteString where
  isNull = LBS.null

instance IsNull (Set.Set a) where
  isNull = Set.null

instance IsNull IS.IntSet where
  isNull = IS.null

instance F.Foldable f => IsNull (f a) where
  isNull = F.foldr (\_ _ -> False) True

