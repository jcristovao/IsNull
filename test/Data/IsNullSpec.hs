{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.IsNullSpec(spec) where

import Data.IsNull

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()
import VectorInstances()

import Data.Maybe
#if !MIN_VERSION_base(4,7,0)
import Data.Either.Compat
#else
import Data.Either
#endif
import Data.Text (Text)


import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.IntSet     as IS
import qualified Data.Vector     as V
import qualified Filesystem.Path.CurrentOS as FP

import qualified Data.List          as L
import qualified Data.Set           as Set
import qualified Data.Map           as Map
import qualified Data.IntMap        as IM
import qualified Data.Sequence      as Seq
import qualified Data.HashSet       as HS
import qualified Data.HashMap.Lazy  as HML
import qualified Data.HashMap.Strict as HMS
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
  describe "IsNull" $ do
    it "Equals null for lists (1)" $ property $ \(x::[Int]) -> isNull x == L.null x
    it "Equals null for lists (2)" $ property $ \(x::[String]) -> isNull x == L.null x
    it "Equals null for lists (3)" $ property $ \(x::[Text]) -> isNull x == L.null x

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

