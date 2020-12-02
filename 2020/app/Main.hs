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

import qualified Day01
import qualified Day02

newtype Day = Day Int

data Part = One | Two

getRunner :: Day -> Part -> Maybe (String -> String)
getRunner (Day 2) One = Just Day02.part1
getRunner (Day 2) Two = Just Day02.part2
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

main :: IO ()
main = do
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
