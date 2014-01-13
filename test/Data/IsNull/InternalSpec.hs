{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNull.InternalSpec (spec) where

import Data.IsNull.Internal

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()
import VectorInstances()

import Data.Maybe
import Data.Text (Text)

import qualified Data.List          as L
import qualified Data.Set           as Set
import qualified Data.Map           as Map
import qualified Data.IntMap        as IM
import qualified Data.Sequence      as Seq
import qualified Data.HashSet       as HS
import qualified Data.HashMap.Lazy  as HML
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector        as V
import qualified Filesystem.Path.CurrentOS as FP

import Control.Applicative

-- the ' avoids conflits with Base 4.7
isLeft' :: Either a b -> Bool
isLeft' = either (const True) (const False)

fpToText :: FP.FilePath -> Text
fpToText = either id id . FP.toText

instance Arbitrary FP.FilePath where
    arbitrary = FP.fromText <$> arbitrary
    shrink xs = FP.fromText <$> shrink (fpToText xs)

instance CoArbitrary FP.FilePath where
    coarbitrary = coarbitrary . fpToText

{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsNull" $ do
    it "Equals null for lists" $ do
      property $ \(x::[Int]) -> isNull x == L.null x
      property $ \(x::[String]) -> isNull x == L.null x
      property $ \(x::[Text]) -> isN x == L.null x


    it "Equals Data.Maybe.isJust for Maybe" $ do
      property $ \(x :: Maybe Int) -> isNull x == isNothing x
      property $ \(x :: Maybe String) -> isNull x == isNothing x

    it "Equals Data.Either.isRight (GHC 7.8) for Either" $ do
      property $ \(x :: Either String String) -> isNull x == isLeft' x

    it "Equals Data.Set.null for Set" $ do
      property $ \(x :: Set.Set String) -> isNull x == Set.null x
      property $ \(x :: Set.Set Int) -> isNull x == Set.null x

    it "Equals Data.Map.null for Map" $ do
      property $ \(x :: Map.Map Int String) -> isNull x == Map.null x
      property $ \(x :: Map.Map String String) -> isNull x == Map.null x
      property $ \(x :: Map.Map Int Int) -> isNull x == Map.null x

    it "Equals Data.IntMap.null for IntMap" $ do
      property $ \(x :: IM.IntMap String) -> isNull x == IM.null x
      property $ \(x :: IM.IntMap Int) -> isNull x == IM.null x

    it "Equals Data.Sequence.null for Sequence" $ do
      property $ \(x  :: Seq.Seq String) -> isNull x == Seq.null x
      property $ \(x  :: Seq.Seq Int   ) -> isNull x == Seq.null x

    it "Equals Data.HashSet.null for HashSet" $ do
      property $ \(x :: HS.HashSet String)  -> isNull x == HS.null x
      property $ \(x :: HS.HashSet Int   )  -> isNull x == HS.null x

    it "Equals Data.HashMap.null for HashMap" $ do
      property $ \(x :: HML.HashMap String String) -> isNull x == HML.null x
      property $ \(x :: HMS.HashMap String String) -> isNull x == HMS.null x
      property $ \(x :: HML.HashMap String Int   ) -> isNull x == HML.null x
      property $ \(x :: HML.HashMap Int    Int   ) -> isNull x == HML.null x

    it "Equals Data.Vector.null for Vector" $ do
      property $ \(x :: V.Vector String) -> isNull x == V.null x

