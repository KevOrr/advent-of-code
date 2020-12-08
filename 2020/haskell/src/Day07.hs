module Day07 where

import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Applicative ((<|>))
import Text.Parsec (Parsec, many1, parse, oneOf, optional, try)
import Text.Parsec.Char (char, digit, letter, spaces, string)

data FlatBag = FlatBag
  { name :: (String, String)
  , children :: [(Int, (String, String))]
  } deriving (Eq, Ord, Show)

parents :: M.Map (String, String) FlatBag -> (String, String) -> S.Set (String, String)
parents bags me =
  M.foldr
  (\b -> if | me `elem` (snd <$> children b) -> (name b `S.insert` parents bags (name b) `S.union`)
            | otherwise -> id)
  S.empty
  bags

bagSize :: M.Map (String, String) FlatBag -> (String, String) -> Int
bagSize bags me =
  M.foldr
  (\b -> if | name b == me -> (+) $ foldl' (\s (n, b) -> s + n * bagSize bags b) 0 (children b)
            | otherwise -> id)
  1
  bags

parseChild :: Parsec String () (Int, (String, String))
parseChild =
  (,)
  <$> (spaces *> (read @Int <$> many1 digit))
  <*> ((,) <$> (spaces *> many1 letter) <*> (spaces *> many1 letter))
  <* spaces <* string "bag" <* optional (char 's') <* oneOf ",."

parseBag :: Parsec String () FlatBag
parseBag =
  FlatBag
  <$> ((,) <$> (spaces *> many1 letter) <*> (spaces *> many1 letter))
  <* (spaces <* string "bags")
  <* (spaces <* string "contain")
  <*> (many1 (try parseChild) <|> (spaces *> string "no other bags." *> pure []))
  <* char '\n'

bagsMap :: [FlatBag] -> M.Map (String, String) FlatBag
bagsMap = foldl' (\s b -> M.insert (name b) b s) M.empty

part1 :: String -> String
part1 = either show (show . length . flip parents ("shiny", "gold") . bagsMap) . parse (many1 parseBag) ""

part2 :: String -> String
part2 = either show (show . subtract 1 . flip bagSize ("shiny", "gold") . bagsMap) . parse (many1 parseBag) ""
