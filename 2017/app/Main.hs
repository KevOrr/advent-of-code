module Main where

import Data.Char

import qualified Day01

newtype Day = Day Int

data Part = One | Two

getRunner :: Day -> Part -> Maybe (String -> String)
getRunner (Day 1) One = Just Day01.run1
getRunner _ _ = Nothing

parseDay :: String -> Either String Day
parseDay s@(read @Int -> Just n)
  | n >= 1 && n <= 25 = Right $ Day n
  -- | otherwise = Right $ "Not an integer between 1 and 25: " <> s
parseDay _ = Left $ "Not an integer between 1 and 25: " <> s

parsePart :: String -> Either String Part
parsePart s
  | toLower s `elem` ["1", "one"] = Right One
  | toLower s `elem` ["2", "two"] = Right Two
  | otherwise = Left $ "Not \"one\" (or \"1\") or \"two\" (or \"2\"): " <> show s

main :: IO ()
main = do
  executable <- getProgName
  args <- getArgs
  if length args != 2 then do
    putStrLn "USAGE: " <> executable <> " DAY PART"
    exitWith 1
  else do
    let Right day = parseDay (args ! 0)
        Right part = parsePart (args ! 1)
        Just runner = getRunner day part
    getContents >>= putStrLn . runner
