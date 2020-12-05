import Test.Hspec

import qualified AdventUtil.Util

spec :: Spec
spec = do
  context "foo" $
    it "asdf" $ True

main :: IO ()
main = hspec spec
