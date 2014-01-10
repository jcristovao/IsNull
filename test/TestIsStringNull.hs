{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestIsStringNull (specs) where

import Data.IsNull

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Filesystem.Path.CurrentOS as FP

import Control.Applicative

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
  describe "IsNull IsString" $ do
    it "Equals Data.Text.empty for Text" $ do
      property $ \(x :: Text) -> isNull x == T.null x

    it "Equals Data.ByteString.empty for ByteString" $ do
      property $ \(x :: ByteString) -> isNull x == BS.null x

    it "Equals Filesystem.Path for System-Filepath" $ do
      property $ \(x :: FP.FilePath) -> isNull x == FP.null x

  describe "IsNNull IsString (nested isNull)" $ do
    it "Handles non null lists of non null foldables the same (returns False)" $ do
      isNNull (["abc"]::[String]) `shouldBe` False
      isNNull (["abc"]::[Text  ]) `shouldBe` False
      isNNull (["","a"]::[String]) `shouldBe` False
      isNNull (Just "a"::Maybe String) `shouldBe` False

    it "Handles non empty lists of null foldable elements (1) (returns True)" $ do
      isNNull ([""] :: [String]) `shouldBe` True
      isNNull ([""] :: [Text  ]) `shouldBe` True
      isNNull (Just ""::Maybe Text) `shouldBe` True

    it "Handles non empty lists of null foldable elements (2) (returns True)" $ do
      isNNull (["",""]::[String])  `shouldBe` True
      isNNull (["",""]::[Text])  `shouldBe` True

