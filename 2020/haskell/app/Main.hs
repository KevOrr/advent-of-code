module Main where

import Text.Read (
  readMaybe
  )
import Data.Char
import System.Environment
import System.Exit
  ( exitWith
  , ExitCode
    ( ExitFailure
    )
  )
import qualified Criterion.Main as C

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day15

data Part = One | Two
newtype Day = Day Int

getRunner :: Day -> Part -> Maybe (String -> String)
getRunner (Day 1) One = Just Day01.part1
getRunner (Day 1) Two = Just Day01.part2
getRunner (Day 2) One = Just Day02.part1
getRunner (Day 2) Two = Just Day02.part2
getRunner (Day 3) One = Just Day03.part1
getRunner (Day 3) Two = Just Day03.part2
getRunner (Day 4) One = Just Day04.part1
getRunner (Day 4) Two = Just Day04.part2
getRunner (Day 5) One = Just Day05.part1
getRunner (Day 5) Two = Just Day05.part2
getRunner (Day 6) One = Just Day06.part1
getRunner (Day 6) Two = Just Day06.part2
getRunner (Day 9) One = Just Day09.part1
getRunner (Day 9) Two = Just Day09.part2
getRunner (Day 10) One = Just Day10.part1
getRunner (Day 10) Two = Just Day10.part2
getRunner (Day 11) One = Just Day11.part1
getRunner (Day 11) Two = Just Day11.part2
getRunner (Day 12) One = Just Day12.part1
getRunner (Day 12) Two = Just Day12.part2
getRunner (Day 15) One = Just Day15.part1
getRunner (Day 15) Two = Just Day15.part2
-- getRunner (Day 7) Two = Just Day07.part2
getRunner _ _ = Nothing

parseDay :: String -> Either String Day
parseDay s@(readMaybe @Int -> Just n)
  | n >= 1 && n <= 25 = Right $ Day n
  -- | otherwise = Right $ "Not an integer between 1 and 25: " <> s
parseDay s = Left $ "Not an integer between 1 and 25: " <> s

parsePart :: String -> Either String Part
parsePart s
  | (toLower <$> s) `elem` ["1", "one"] = Right One
  | (toLower <$> s) `elem` ["2", "two"] = Right Two
  | otherwise = Left $ "Not \"one\" (or \"1\") or \"two\" (or \"2\"): " <> show s

run :: IO ()
run = do
  executable <- getProgName
  args <- getArgs
  if length args /= 2 then do
    putStrLn $ "USAGE: " <> executable <> " DAY PART"
    exitWith $ ExitFailure 1
  else do
    let Right day = parseDay (args !! 0)
        Right part = parsePart (args !! 1)
        Just runner = getRunner day part
    getContents >>= putStrLn . runner

main :: IO ()
main = run
--main = C.defaultMain [ C.bench "part1" . C.nfIO $ readFile "f" >>= putStrLn . Day07.part1]
