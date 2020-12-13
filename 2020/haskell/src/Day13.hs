module Day13 where

import Data.Ord
import Data.List
import Control.Applicative
import Text.Parsec (Parsec, parse)
import Text.Parsec.Combinator
import Text.Parsec.Char

egcd :: Integral a => a -> a -> (a, a)
egcd a b = go a b 1 0 0 1
  where
    go r' r s' s t' t
      | r /= 0 =
        let q = r' `div` r in
          go r (r' - q*r) s (s' - q*s) t (t' - q*t)
      | otherwise = (s', t')

chineseRemainder :: Integral a => [(a, a)] -> a
chineseRemainder xs = foo `mod` prod
  where
    foo = sum . fmap (\(a, n) -> a * snd (egcd n (prod `div` n)) * prod `div` n) $ xs
    prod = product . fmap snd $ xs

parser :: (Read a, Integral a) => Parsec String () (a, [(a, a)])
parser = (,) <$> (read <$> many1 digit <* char '\n') <*> parseBuses
  where
    parseBuses = convertBuses <$> ((Just . read <$> many1 digit <|> Nothing <$ char 'x') `sepBy` char ',')
    convertBuses xs = [(-idx, p) | (idx, Just p) <- zip [0..] xs]

part1 :: String -> String
part1 = either show (show . uncurry firstBusTime) . parse (parser @Integer) ""
  where
    firstBusTime t = (\b -> b * busTime t b) . minimumBy (comparing (busTime t)) . fmap snd
    busTime t b = (t + b - 1) `div` b * b - t

part2 :: String -> String
part2 = either show (show . chineseRemainder . snd) . parse (parser @Integer) ""
