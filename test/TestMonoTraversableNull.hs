{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestMonoTraversableNull (specs) where

import Data.IsNull.MonoTraversable

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.IntSet     as IS
import qualified Data.Vector     as V

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
specs :: Spec
specs = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsNull MonoFoldable" $ do
    it "Equals Data.Text.empty for Text" $ do
      property $ \(x :: Text) -> isNull x == T.null x

    it "Equals Data.ByteString.empty for ByteString" $ do
      property $ \(x :: ByteString) -> isNull x == BS.null x

    it "Equals Data.IntSet.null for IntSet" $ do
      property $ \(x :: IS.IntSet) -> isNull x == IS.null x

  describe "IsNNull MonoFoldable (nested isNull)" $ do
    it "Handles non null lists of non null foldables the same (returns False)" $ do
      isNNull (["abc"]::[String]) `shouldBe` False
      isNNull (["abc"]::[Text  ]) `shouldBe` False
      isNNull (["","a"]::[String]) `shouldBe` False
      isNNull (Just "a"::Maybe String) `shouldBe` False
      isNNull (Right "a":: Either Int String) `shouldBe` False

    it "Handles non empty lists of null foldable elements (1) (returns True)" $ do
      isNNull ([""] :: [String]) `shouldBe` True
      isNNull ([""] :: [Text  ]) `shouldBe` True
      isNNull (Just ""::Maybe Text) `shouldBe` True
      isNNull (Right "":: Either Int String) `shouldBe` True

    it "Handles non empty lists of null foldable elements (2) (returns True)" $ do
      isNNull (["",""]::[String])  `shouldBe` True
      isNNull (["",""]::[Text])  `shouldBe` True

    it "Handles non empty lists of null foldable elements (3) (returns True)" $ do
      isNNull (V.fromList ["",""]::V.Vector String)  `shouldBe` True
      isNNull (V.fromList ["",""]::V.Vector Text)  `shouldBe` True


