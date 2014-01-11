import Test.Hspec

import qualified TestCommonNull          as TCE
import qualified TestMonoTraversableNull as TMNE
import qualified TestIsStringNull        as TISE
import qualified TestMonoidNull          as TMID

main :: IO ()
main = hspec $ do
  TCE.specs
  TMNE.specs
  TISE.specs
  TMID.specs
