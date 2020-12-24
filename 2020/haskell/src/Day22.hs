module Day22 where

import Control.Monad
import qualified Data.HashSet as HS
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Deck = [Int]
type Deck2 = (Deck, Deck)

parsePlayers :: Parsec Void String Deck2
parsePlayers = do
  void (symbol "Player 1:")
  p1 <- some (lexeme L.decimal)
  void (symbol "Player 2:")
  p2 <- some (lexeme L.decimal)
  pure (p1, p2)
  where
    lexeme = L.lexeme space
    symbol = L.symbol space

part1Rules :: Deck2 -> (Int, Deck)
part1Rules (winner, []) = (1, winner)
part1Rules ([], winner) = (2, winner)
part1Rules (x : xs, y : ys)
  | x > y = part1Rules (xs ++ [x, y], ys)
  | otherwise = part1Rules (xs, ys ++ [y, x])

part2Rules :: Deck2 -> (Int, Deck)
part2Rules = fst . go HS.empty HS.empty
  where
    go hist1 hist2 (winner, []) = ((1, winner), (hist1, hist2))
    go hist1 hist2 ([], winner) = ((2, winner), (hist1, hist2))
    go hist1 hist2 (x : xs, y : ys)
      | (x : xs) `HS.member` hist1 || (y : ys) `HS.member` hist2 = ((1, x : xs), (hist1, hist2))
      | length xs >= x && length ys >= y =
        case go HS.empty HS.empty (take x xs, take y ys) of
          ((1, _), _) -> go hist1' hist2' (xs ++ [x, y], ys)
          ((_, _), _) -> go hist1' hist2' (xs, ys ++ [y, x])
      | x > y = go hist1' hist2' (xs ++ [x, y], ys)
      | otherwise = go hist1' hist2' (xs, ys ++ [y, x])
      where
        hist1' = HS.insert (x : xs) hist1
        hist2' = HS.insert (y : ys) hist2

score :: Deck -> Integer
score = sum . zipWith (\x y -> x * toInteger y) [1 ..] . reverse

main :: IO ()
main = do
  input <- readFile "src/input"
  let (deck1, deck2) = either (error . errorBundlePretty) id . parse (parsePlayers <* eof) "" $ input
      (_, part1Deck) = part1Rules (deck1, deck2)
      (_, part2Deck) = part2Rules (deck1, deck2)
  print (score part1Deck)
  print (score part2Deck)
