{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestCommonEmpty (specs) where

import Data.IsEmpty.Internal

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import VectorInstances

import Data.Maybe
import Data.Text (Text)

import qualified Data.List          as L
import qualified Data.Set           as Set
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



{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
specs :: Spec
specs = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsEmpty" $ do
    it "Equals null for lists" $ do
      property $ \(x::[Int]) -> isEmpty x == L.null x
      property $ \(x::[String]) -> isEmpty x == L.null x
      property $ \(x::[Text]) -> isEmpty x == L.null x

    it "Equals Data.Maybe.isJust for Maybe" $ do
      property $ \(x :: Maybe Int) -> isEmpty x == isNothing x

    it "Equals Data.Either.isRight (GHC 7.8) for Either" $ do
      property $ \(x :: Either String String) -> isEmpty x == isLeft' x

    it "Equals Data.Set.null for Set" $ do
      property $ \(x :: Set.Set String) -> isEmpty x == Set.null x
      property $ \(x :: Set.Set Int) -> isEmpty x == Set.null x

    it "Equals Data.IntMap.null for IntMap" $ do
      property $ \(x :: IM.IntMap String) -> isEmpty x == IM.null x
      property $ \(x :: IM.IntMap Int) -> isEmpty x == IM.null x

    it "Equals Data.Sequence.null for Sequence" $ do
      property $ \(x  :: Seq.Seq String) -> isEmpty x == Seq.null x
      property $ \(x  :: Seq.Seq Int   ) -> isEmpty x == Seq.null x

    it "Equals Data.HashSet.null for HashSet" $ do
      property $ \(x :: HS.HashSet String)  -> isEmpty x == HS.null x
      property $ \(x :: HS.HashSet Int   )  -> isEmpty x == HS.null x

    it "Equals Data.HashMap.null for HashMap" $ do
      property $ \(x :: HML.HashMap String String) -> isEmpty x == HML.null x
      property $ \(x :: HMS.HashMap String String) -> isEmpty x == HMS.null x
      property $ \(x :: HML.HashMap String Int   ) -> isEmpty x == HML.null x
      property $ \(x :: HML.HashMap Int    Int   ) -> isEmpty x == HML.null x

    it "Equals Data.Vector.null for Vector" $ do
      property $ \(x :: V.Vector String) -> isEmpty x == V.null x
