import Test.Hspec

import qualified TestCommonEmpty          as TCE
import qualified TestMonoTraversableEmpty as TMNE
import qualified TestIsStringEmpty        as TISE

main :: IO ()
main = hspec $ do
  TCE.specs
  TMNE.specs
  TISE.specs
