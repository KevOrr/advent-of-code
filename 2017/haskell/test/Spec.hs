import Test.Hspec

import qualified Day01

spec :: Spec
spec = do
  context "Day 01" $ do
    describe "part 1" $ do
      it "\"1122\" -> 3" $ Day01.run1 "1122" `shouldBe` "3"
      it "\"1111\" -> 4" $ Day01.run1 "1111" `shouldBe` "4"
      it "\"1234\" -> 0" $ Day01.run1 "1234" `shouldBe` "0"
      it "\"91212129\" -> 9" $ Day01.run1 "91212129" `shouldBe` "9"

main :: IO ()
main = hspec spec
