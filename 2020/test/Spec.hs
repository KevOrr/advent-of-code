import Test.Hspec

import Data.List (sort)
import Data.Maybe (fromMaybe)

import qualified Day01
import qualified Day02
import qualified Day03

spec :: Spec
spec = do
  describe "Day 01" $ do
    let test = [1721, 979, 366, 299, 675, 1456]
    context "Part 1" $ do
      it "works on example input" $
        (sort . fromMaybe [] $ Day01.sum2 2020 test) `shouldBe` [299, 1721]
    context "Part 2" $ do
      it "works on example input" $
        (sort . fromMaybe [] $ Day01.sumn 3 2020 test) `shouldBe` [366, 675, 979]

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

  describe "Day 03" $ do
    let test = "..##.......\n\
               \#...#...#..\n\
               \.#....#..#.\n\
               \..#.#...#.#\n\
               \.#...##..#.\n\
               \..#.##.....\n\
               \.#.#.#....#\n\
               \.#........#\n\
               \#.##...#...\n\
               \#...##....#\n\
               \.#..#...#.#"
    context "Part 1" $ do
      it "works on example input" $ do
        Day03.part1 test `shouldBe` "7"
    context "Part 2" $ do
      it "works on example input" $ do
        Day03.part2 test `shouldBe` "336"

main :: IO ()
main = hspec spec
