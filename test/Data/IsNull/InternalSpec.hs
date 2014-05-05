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
import Data.Either.Compat
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
    it "Equals null for lists (1)" $ property $ \(x::[Int]) -> isNull x == L.null x
    it "Equals null for lists (2)" $ property $ \(x::[String]) -> isNull x == L.null x
    it "Equals null for lists (3)" $ property $ \(x::[Text]) -> isN x == L.null x

    it "Equals Data.Maybe.isJust for Maybe (1)" $ property $ \(x :: Maybe Int) -> isNull x == isNothing x
    it "Equals Data.Maybe.isJust for Maybe (2)" $ property $ \(x :: Maybe String) -> isNull x == isNothing x

    it "Equals Data.Either.isRight (GHC 7.8) for Either (1)" $ property $ \(x :: Either String String) -> isNull x == isLeft x

    it "Equals Data.Set.null for Set (1)" $ property $ \(x :: Set.Set String) -> isNull x == Set.null x
    it "Equals Data.Set.null for Set (2)" $ property $ \(x :: Set.Set Int) -> isNull x == Set.null x

    it "Equals Data.Map.null for Map (1)" $ property $ \(x :: Map.Map Int String) -> isNull x == Map.null x
    it "Equals Data.Map.null for Map (2)" $ property $ \(x :: Map.Map String String) -> isNull x == Map.null x
    it "Equals Data.Map.null for Map (3)" $ property $ \(x :: Map.Map Int Int) -> isNull x == Map.null x

    it "Equals Data.IntMap.null for IntMap (1)" $ property $ \(x :: IM.IntMap String) -> isNull x == IM.null x
    it "Equals Data.IntMap.null for IntMap (2)" $ property $ \(x :: IM.IntMap Int) -> isNull x == IM.null x

    it "Equals Data.Sequence.null for Sequence (1)" $ property $ \(x  :: Seq.Seq String) -> isNull x == Seq.null x
    it "Equals Data.Sequence.null for Sequence (2)" $ property $ \(x  :: Seq.Seq Int   ) -> isNull x == Seq.null x

    it "Equals Data.HashSet.null for HashSet (1)" $ property $ \(x :: HS.HashSet String)  -> isNull x == HS.null x
    it "Equals Data.HashSet.null for HashSet (2)" $ property $ \(x :: HS.HashSet Int   )  -> isNull x == HS.null x

    it "Equals Data.HashMap.null for HashMap (1)" $ property $ \(x :: HML.HashMap String String) -> isNull x == HML.null x
    it "Equals Data.HashMap.null for HashMap (2)" $ property $ \(x :: HMS.HashMap String String) -> isNull x == HMS.null x
    it "Equals Data.HashMap.null for HashMap (3)" $ property $ \(x :: HML.HashMap String Int   ) -> isNull x == HML.null x
    it "Equals Data.HashMap.null for HashMap (4)" $ property $ \(x :: HML.HashMap Int    Int   ) -> isNull x == HML.null x

    it "Equals Data.Vector.null for Vector" $ do
      property $ \(x :: V.Vector String) -> isNull x == V.null x

