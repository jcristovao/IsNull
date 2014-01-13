import Test.Hspec

import qualified TestCommonNull          as TCE
import qualified TestMonoTraversableNull as TMNE
import qualified TestIsStringNull        as TISE
import qualified TestMonoidNull          as TMID
import qualified TestMonadPlusNull       as TMPN

main :: IO ()
main = hspec $ do
  TCE.specs
  TMNE.specs
  TISE.specs
  TMID.specs
  TMPN.specs
