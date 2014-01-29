{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNullSpec(spec) where

import Data.IsNull

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.IntSet     as IS
import qualified Data.Vector     as V
import qualified Filesystem.Path.CurrentOS as FP

import Control.Applicative

fpToText :: FP.FilePath -> Text
fpToText = either id id . FP.toText

instance Arbitrary FP.FilePath where
    arbitrary = FP.fromText <$> arbitrary
    shrink xs = FP.fromText <$> shrink (fpToText xs)

instance CoArbitrary FP.FilePath where
    coarbitrary = coarbitrary . fpToText

instance IsNull FP.FilePath where
  isNull = FP.null


{-# ANN spec ("HLint: ignore Redundant do"::String) #-}
spec :: Spec
spec = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsNull" $ do
    it "Equals Data.Text.empty for Text" $ do
      property $ \(x :: Text) -> isNull x == T.null x

    it "Equals Data.ByteString.empty for ByteString" $ do
      property $ \(x :: ByteString) -> isNull x == BS.null x

    it "Equals Data.IntSet.null for IntSet" $ do
      property $ \(x :: IS.IntSet) -> isNull x == IS.null x

    it "Equals Filesystem.Path for System-Filepath" $ do
      property $ \(x :: FP.FilePath) -> isNull x == FP.null x

  describe "IsNullN (nested isNull)" $ do
    it "Handles non null lists of non null foldables the same (returns False)" $ do
      isNullN (["abc"]::[String]) `shouldBe` False
      isNullN (["abc"]::[Text  ]) `shouldBe` False
      isNullN (["","a"]::[String]) `shouldBe` False
      isNullN (Just "a"::Maybe String) `shouldBe` False
      isNullN (Right "a":: Either Int String) `shouldBe` False

    it "Handles non-null 'empty' values when applicable (either)" $ do
      isNullN (Left "a" :: Either String String) `shouldBe` True
      isNullN (Left ""  :: Either String String) `shouldBe` True
      isNullN (Right ""  :: Either String String) `shouldBe` True

    it "Handles non empty lists of null foldable elements (1) (returns True)" $ do
      isNullN ([""] :: [String]) `shouldBe` True
      isNullN ([""] :: [Text  ]) `shouldBe` True
      isNullN (Just ""::Maybe Text) `shouldBe` True
      isNullN (Right "":: Either Int String) `shouldBe` True

    it "Handles non empty lists of null foldable elements (2) (returns True)" $ do
      isNullN (["",""]::[String])  `shouldBe` True
      isNullN (["",""]::[Text])  `shouldBe` True

    it "Handles non empty lists of null foldable elements (3) (returns True)" $ do
      isNullN (V.fromList ["",""]::V.Vector String)  `shouldBe` True
      isNullN (V.fromList ["",""]::V.Vector Text)  `shouldBe` True

  describe "IsNullM (Monadic)" $ do
    it "Equals null for lists" $ do
      (isNullNM . return $ (["aaa"] :: [String])) `shouldReturn` False
      (isNullNM . return $ ([""]    :: [String])) `shouldReturn` True
      (isNullNM . return $ ([]      :: [String])) `shouldReturn` True
      (isNullNM . return $ (["aaa"] :: [Text]))   `shouldReturn` False
      (isNullNM . return $ ([""]    :: [Text]))   `shouldReturn` True
      (isNullNM . return $ ([]      :: [Text]))   `shouldReturn` True


    it "Equals Data.Maybe.isJust for Maybe" $ do
      (isNullNM . return $ (Just "aaa" :: Maybe String)) `shouldReturn` False
      (isNullNM . return $ (Just ""    :: Maybe String)) `shouldReturn` True
      (isNullNM . return $ (Nothing    :: Maybe String)) `shouldReturn` True
      (isNullNM . return $ (Just "aaa" :: Maybe Text))   `shouldReturn` False
      (isNullNM . return $ (Just ""    :: Maybe Text))   `shouldReturn` True
      (isNullNM . return $ (Nothing    :: Maybe Text))   `shouldReturn` True


    it "Equals Data.Either.isRight (GHC 7.8) for Either" $ do
      (isNullNM . return $ (Right "aaa" :: Either String String)) `shouldReturn` False
      (isNullNM . return $ (Right ""    :: Either String String)) `shouldReturn` True
      (isNullNM . return $ (Left  "aaa" :: Either String String)) `shouldReturn` True
      (isNullNM . return $ (Left  ""    :: Either String String)) `shouldReturn` True
      (isNullNM . return $ (Right "aaa" :: Either Text Text))     `shouldReturn` False
      (isNullNM . return $ (Right ""    :: Either Text Text))     `shouldReturn` True
      (isNullNM . return $ (Left  "aaa" :: Either Text Text))     `shouldReturn` True
      (isNullNM . return $ (Left  ""    :: Either Text Text))     `shouldReturn` True

