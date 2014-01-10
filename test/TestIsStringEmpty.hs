{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestIsStringEmpty (specs) where

import Data.IsEmpty

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
  describe "IsEmpty IsString" $ do
    it "Equals Data.Text.empty for Text" $ do
      property $ \(x :: Text) -> isEmpty x == T.null x

    it "Equals Data.ByteString.empty for ByteString" $ do
      property $ \(x :: ByteString) -> isEmpty x == BS.null x

    it "Equals Filesystem.Path for System-Filepath" $ do
      property $ \(x :: FP.FilePath) -> isEmpty x == FP.null x
