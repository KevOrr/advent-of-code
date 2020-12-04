import Test.Hspec

import Data.List (sort)
import Data.Maybe (fromMaybe)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04

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

  describe "Day 04" $ do
    let test = unlines [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
                       , "byr:1937 iyr:2017 cid:147 hgt:183cm"
                       , ""
                       , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
                       , "hcl:#cfa07d byr:1929"
                       , ""
                       , "hcl:#ae17e1 iyr:2013"
                       , "eyr:2024"
                       , "ecl:brn pid:760753108 byr:1931"
                       , "hgt:179cm"
                       , ""
                       , "hcl:#cfa07d eyr:2025 pid:166559648"
                       , "iyr:2011 ecl:brn hgt:59in"
                       ]
    context "Part 1" $ do
      it "works on example input" $ do
        Day04.part1 test `shouldBe` "2"
    context "Part 2" $ do
      it "byr:2002 is valid" $ do
        Day04.validate "byr" "2002" `shouldBe` True
      it "byr:2003 is invalid" $ do
        Day04.validate "byr" "2003" `shouldBe` False

      it "hgt:60in is valid" $ do
        Day04.validate "hgt" "60in" `shouldBe` True
      it "hgt:190cm is valid" $ do
        Day04.validate "hgt" "190cm" `shouldBe` True
      it "hgt:190in is invalid" $ do
        Day04.validate "hgt" "190in" `shouldBe` False
      it "hgt:190 is invalid" $ do
        Day04.validate "hgt" "190" `shouldBe` False

      it "hcl:#213abc is valid" $ do
        Day04.validate "hcl" "#123abc" `shouldBe` True
      it "hcl:#123abz is invalid" $ do
        Day04.validate "hcl" "#123abz" `shouldBe` False
      it "hcl:123abc is invalid" $ do
        Day04.validate "hcl" "123abc" `shouldBe` False

      it "ecl:brn is valid" $ do
        Day04.validate "ecl" "brn" `shouldBe` True
      it "ecl:wat is invalid" $ do
        Day04.validate "ecl" "wat" `shouldBe` False

      it "pid:000000001 is valid" $ do
        Day04.validate "pid" "000000001" `shouldBe` True
      it "pid:0123456789 is invalid" $ do
        Day04.validate "pid" "0123456789" `shouldBe` False

      it "cid:_|_ is valid" $ do
        Day04.validate "cid" undefined `shouldBe` True

main :: IO ()
main = hspec spec
