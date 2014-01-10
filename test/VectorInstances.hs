{-# OPTIONS_GHC -fno-warn-orphans #-}
module VectorInstances where

import Test.QuickCheck

import qualified Data.Vector as DV
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
{-import qualified Data.Vector.Fusion.Bundle as S-}

instance Arbitrary a => Arbitrary (DV.Vector a) where
    arbitrary = fmap DV.fromList arbitrary

instance CoArbitrary a => CoArbitrary (DV.Vector a) where
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, DVP.Prim a) => Arbitrary (DVP.Vector a) where
    arbitrary = fmap DVP.fromList arbitrary

instance (CoArbitrary a, DVP.Prim a) => CoArbitrary (DVP.Vector a) where
    coarbitrary = coarbitrary . DVP.toList

instance (Arbitrary a, DVS.Storable a) => Arbitrary (DVS.Vector a) where
    arbitrary = fmap DVS.fromList arbitrary

instance (CoArbitrary a, DVS.Storable a) => CoArbitrary (DVS.Vector a) where
    coarbitrary = coarbitrary . DVS.toList

instance (Arbitrary a, DVU.Unbox a) => Arbitrary (DVU.Vector a) where
    arbitrary = fmap DVU.fromList arbitrary

instance (CoArbitrary a, DVU.Unbox a) => CoArbitrary (DVU.Vector a) where
    coarbitrary = coarbitrary . DVU.toList


