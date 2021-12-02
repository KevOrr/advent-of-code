import Test.Hspec

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Text.Printf (printf)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

spec :: Spec
spec = do
  fdescribe "Day 01" $ do
    -- let test = [1721, 979, 366, 299, 675, 1456]
    context "Part 1" $ do
      it "works on example input" $ True
        -- (sort . fromMaybe [] $ Day01.sum2 2020 test) `shouldBe` [299, 1721]
    context "Part 2" $ do
      it "works on example input" $ True
        -- (sort . fromMaybe [] $ Day01.sumn 3 2020 test) `shouldBe` [366, 675, 979]

  xdescribe "Day 02" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 03" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 04" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 5" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 6" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 7" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 8" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 9" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 10" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 11" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 12" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 13" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 14" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 15" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 16" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 17" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 18" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 19" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 20" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 21" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 22" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 23" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 24" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

  xdescribe "Day 25" $ do
    context "Part 1" $ do
      it "works on example input" $ True
    context "Part 2" $ do
      it "works on example input" $ True

main :: IO ()
main = hspec spec
