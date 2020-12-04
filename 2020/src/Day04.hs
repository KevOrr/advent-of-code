module Day04 where

import Text.Read (readMaybe)
import Data.Either (isLeft, isRight)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Text.Parsec (Parsec, many, many1, parse, oneOf, eof)
import Text.Parsec.Char (char, digit, letter, space, spaces, alphaNum)
import Text.Parsec.Combinator (sepBy, endBy, count)
import qualified Data.Set as S

parser :: Parsec String () [[(String, String)]]
parser = (<* eof) $ flip sepBy (char '\n' *> many (char '\n')) $
  flip endBy (void (many1 $ char ' ') <|> void (char '\n')) $
    (,) <$> (many1 letter <* char ':') <*> many1 (alphaNum <|> char '#')

validate :: String -> String -> Bool
validate = \case
  "byr" -> maybe False (\n -> n >= 1920 && n <= 2002) . readMaybe
  "iyr" -> maybe False (\n -> n >= 2010 && n <= 2020) . readMaybe
  "eyr" -> maybe False (\n -> n >= 2020 && n <= 2030) . readMaybe
  "hgt" -> either (const False)
                  (\case
                      (Just n, "cm") -> n >= 150 && n <= 193
                      (Just n, "in") -> n >= 59 && n <= 76
                      _ -> False)
           . parse ((,) <$> (readMaybe <$> many1 digit) <*> many1 letter <* eof) ""
  "hcl" -> isRight . parse (char '#' *> count 6 (oneOf "abcdef0123456789") <* eof) ""
  "ecl" -> flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  "pid" -> isRight . parse (count 9 digit <* eof) ""
  "cid" -> const True
  _ -> const False

fieldsPresent :: [(String, a)] -> Bool
fieldsPresent = S.isSubsetOf (S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) . S.fromList . fmap fst

part1 :: String -> String
part1 = either show (show . length . filter fieldsPresent) . parse parser ""

part2 :: String -> String
part2 = either show (show . length . filter (\p -> all (uncurry validate) p && fieldsPresent p)) . parse parser ""
