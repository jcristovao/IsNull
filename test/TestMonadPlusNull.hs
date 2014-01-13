{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestMonadPlusNull (specs) where

import Data.IsNull
import Data.IsNull.MonadPlus

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import VectorInstances

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

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
specs :: Spec
specs = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsNullM (Monadic)" $ do
    it "Equals null for lists" $ do
      (isNullM . return $ (["aaa"] :: [String])) `shouldReturn` False
      (isNullM . return $ ([""]    :: [String])) `shouldReturn` True
      (isNullM . return $ ([]      :: [String])) `shouldReturn` True
      (isNullM . return $ (["aaa"] :: [Text])) `shouldReturn` False
      (isNullM . return $ ([""]    :: [Text])) `shouldReturn` True
      (isNullM . return $ ([]      :: [Text])) `shouldReturn` True


    it "Equals Data.Maybe.isJust for Maybe" $ do
      (isNullM . return $ (Just "aaa" :: Maybe String)) `shouldReturn` False
      (isNullM . return $ (Just ""    :: Maybe String)) `shouldReturn` True
      (isNullM . return $ (Nothing    :: Maybe String)) `shouldReturn` True
      (isNullM . return $ (Just "aaa" :: Maybe Text)) `shouldReturn` False
      (isNullM . return $ (Just ""    :: Maybe Text)) `shouldReturn` True
      (isNullM . return $ (Nothing    :: Maybe Text)) `shouldReturn` True


    it "Equals Data.Either.isRight (GHC 7.8) for Either" $ do
      (isNullM . return $ (Right "aaa" :: Either String String)) `shouldReturn` False
      (isNullM . return $ (Right ""    :: Either String String)) `shouldReturn` True
      (isNullM . return $ (Left  "aaa" :: Either String String)) `shouldReturn` True
      (isNullM . return $ (Left  ""    :: Either String String)) `shouldReturn` True
      (isNullM . return $ (Right "aaa" :: Either Text Text)) `shouldReturn` False
      (isNullM . return $ (Right ""    :: Either Text Text)) `shouldReturn` True
      (isNullM . return $ (Left  "aaa" :: Either Text Text)) `shouldReturn` True
      (isNullM . return $ (Left  ""    :: Either Text Text)) `shouldReturn` True
      {-rv `shouldBe` False-}
      {-property $ \(x :: Either String String) -> isNullM x == (return . isNNull $ x)-}
      {-isNullM (Right "aaa" :: Either String String) `shouldReturn` False-}

    {-it "Equals Data.Set.null for Set" $ do-}
      {-property $ \(x :: Set.Set String) -> isNullM x == Set.null x-}
      {-property $ \(x :: Set.Set Int) -> isNullM x == Set.null x-}

    {-it "Equals Data.Map.null for Map" $ do-}
      {-property $ \(x :: Map.Map Int String) -> isNullM x == Map.null x-}
      {-property $ \(x :: Map.Map String String) -> isNullM x == Map.null x-}
      {-property $ \(x :: Map.Map Int Int) -> isNullM x == Map.null x-}

    {-it "Equals Data.IntMap.null for IntMap" $ do-}
      {-property $ \(x :: IM.IntMap String) -> isNullM x == IM.null x-}
      {-property $ \(x :: IM.IntMap Int) -> isNullM x == IM.null x-}

    {-it "Equals Data.Sequence.null for Sequence" $ do-}
      {-property $ \(x  :: Seq.Seq String) -> isNullM x == Seq.null x-}
      {-property $ \(x  :: Seq.Seq Int   ) -> isNullM x == Seq.null x-}

    {-it "Equals Data.HashSet.null for HashSet" $ do-}
      {-property $ \(x :: HS.HashSet String)  -> isNullM x == HS.null x-}
      {-property $ \(x :: HS.HashSet Int   )  -> isNullM x == HS.null x-}

    {-it "Equals Data.HashMap.null for HashMap" $ do-}
      {-property $ \(x :: HML.HashMap String String) -> isNullM x == HML.null x-}
      {-property $ \(x :: HMS.HashMap String String) -> isNullM x == HMS.null x-}
      {-property $ \(x :: HML.HashMap String Int   ) -> isNullM x == HML.null x-}
      {-property $ \(x :: HML.HashMap Int    Int   ) -> isNullM x == HML.null x-}

    {-it "Equals Data.Vector.null for Vector" $ do-}
      {-property $ \(x :: V.Vector String) -> isNullM x == V.null x-}

