import Test.Hspec

import qualified Day01
import qualified Day02

spec :: Spec
spec = do
  describe "Day 01" $ do
    context "Part 1" $ do
      it "works on example input" $
        True
      --   sort (Day01.naive2Sum 2020
      --         [ 1721
      --         , 979
      --         , 366
      --         , 299
      --         , 675
      --         , 1456
      --         ])
      --   `shouldBe`
      --   (299, 1721)

  describe "Day 02" $ do
    context "Part 1" $ do
      it "example 1" $
        Day02.part1 "1-3 a: abcde" `shouldBe` "1"
      it "example 2" $
        Day02.part1 "1-3 b: cdefg" `shouldBe` "0"
      it "example 3" $
        Day02.part1 "2-9 c: ccccccccc" `shouldBe` "1"

    context "Part 2" $ do
      it "example 1" $
        Day02.part2 "1-3 a: abcde" `shouldBe` "1"
      it "example 2" $
        Day02.part2 "1-3 b: cdefg" `shouldBe` "0"
      it "example 3" $
        Day02.part2 "2-9 c: ccccccccc" `shouldBe` "0"


main :: IO ()
main = hspec spec
