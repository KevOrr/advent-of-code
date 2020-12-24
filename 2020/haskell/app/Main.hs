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
import qualified Day23

data Part = One | Two
newtype Day = Day Int

getRunner :: Day -> Part -> Maybe (Either (String -> String) (String -> IO ()))
getRunner (Day 1)  One = Just $ Left  Day01.part1
getRunner (Day 1)  Two = Just $ Left  Day01.part2
getRunner (Day 2)  One = Just $ Left  Day02.part1
getRunner (Day 2)  Two = Just $ Left  Day02.part2
getRunner (Day 3)  One = Just $ Left  Day03.part1
getRunner (Day 3)  Two = Just $ Left  Day03.part2
getRunner (Day 4)  One = Just $ Left  Day04.part1
getRunner (Day 4)  Two = Just $ Left  Day04.part2
getRunner (Day 5)  One = Just $ Left  Day05.part1
getRunner (Day 5)  Two = Just $ Left  Day05.part2
getRunner (Day 6)  One = Just $ Left  Day06.part1
getRunner (Day 6)  Two = Just $ Left  Day06.part2
getRunner (Day 9)  One = Just $ Left  Day09.part1
getRunner (Day 9)  Two = Just $ Left  Day09.part2
getRunner (Day 10) One = Just $ Left  Day10.part1
getRunner (Day 10) Two = Just $ Left  Day10.part2
getRunner (Day 11) One = Just $ Right Day11.part1
getRunner (Day 11) Two = Just $ Right Day11.part2
getRunner (Day 12) One = Just $ Left  Day12.part1
getRunner (Day 12) Two = Just $ Left  Day12.part2
getRunner (Day 15) One = Just $ Left  Day15.part1
getRunner (Day 15) Two = Just $ Left  Day15.part2
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
        runner = getRunner day part
    case runner of
      Nothing -> putStrLn "No such day/part"
      Just (Left runnerPure) -> getContents >>= putStrLn . runnerPure
      Just (Right runnerAction) -> getContents >>= runnerAction

main :: IO ()
main = Day23.main
--main = C.defaultMain [ C.bench "part1" . C.nfIO $ readFile "f" >>= putStrLn . Day07.part1]
