{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE CPP                  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.Class where

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
import Control.Monad (liftM)

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

  -- | Typing causes arthritis. Alias for @'isNull'@.
  isN :: a -> Bool
  isN = isNull

  -- | the logical negation of @'isNull'@
  notNull :: a -> Bool
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
  isNullN :: F.Foldable f => f a -> Bool
  isNullN = F.all isNull

  -- | Nested isNotNull
  notNullN :: F.Foldable f => f a -> Bool
  notNullN = not . isNullN

  -- | Monadic isNull
  --
  -- >>> isNullM [""]
  -- [True]
  isNullM :: Functor m => m a -> m Bool
  isNullM = fmap isNull

  -- | Monadic Nested isNull
  isNullNM :: (Functor m, F.Foldable f) => m (f a) -> m Bool
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
  (<\>) :: a -> a -> a
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


