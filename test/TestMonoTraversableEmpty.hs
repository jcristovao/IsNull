{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestMonoTraversableEmpty (specs) where

import Data.IsEmpty.MonoTraversable

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

import Data.ByteString (ByteString)
import Data.Text (Text)

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.IntSet        as IS

{-# ANN specs ("HLint: ignore Redundant do"::String) #-}
specs :: Spec
specs = do
------------------------------------------------------------------------------
-- Maybe ---------------------------------------------------------------------
------------------------------------------------------------------------------
  describe "IsEmpty MonoFoldable" $ do
    it "Equals Data.Text.empty for Text" $ do
      property $ \(x :: Text) -> isEmpty x == T.null x

    it "Equals Data.ByteString.empty for ByteString" $ do
      property $ \(x :: ByteString) -> isEmpty x == BS.null x

    it "Equals Data.IntSet.null for IntSet" $ do
      property $ \(x :: IS.IntSet) -> isEmpty x == IS.null x


