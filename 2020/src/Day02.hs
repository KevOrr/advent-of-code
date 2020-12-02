module Day02 where

import Control.Applicative (liftA)
import Data.Bits (xor)
import Text.Parsec (Parsec, many, parse)
import Text.Parsec.Char (char, digit, letter, spaces)

data Policy = Policy
  { polLow :: Int,
    polHigh :: Int,
    polChar :: Char,
    polPass :: [Char]
  }

parser :: Parsec String () [Policy]
parser =
  many $
    Policy
      <$> (spaces *> liftA read (many digit))
      <*> (char '-' *> liftA read (many digit))
      <*> (spaces *> letter <* char ':')
      <*> (spaces *> many letter <* spaces)

ifParse :: Parsec String () a -> (a -> String) -> String -> String
ifParse p f (parse p "" -> Right x) = f x
ifParse p _ (parse p "" -> Left err) = show err

single :: Int -> [a] -> [a]
single a = take 1 . drop a

checkPolPart1 :: Policy -> Bool
checkPolPart1 p = polLow p <= freq && freq <= polHigh p
  where freq = length . filter (== (polChar p)) $ polPass p

checkPolPart2 :: Policy -> Bool
checkPolPart2 p = (polChar p `elem` single (polLow p - 1) (polPass p)) `xor` (polChar p `elem` single (polHigh p - 1) (polPass p))

part1 :: String -> String
part1 = ifParse parser $ show @Int . length . filter id . fmap checkPolPart1

part2 :: String -> String
part2 = ifParse parser $ show @Int . length . filter id . fmap checkPolPart2
