module Day10 where

import qualified Data.Map.Strict as M
import Data.List (group, sort)
import Data.List.Split (splitOn)

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = M.fromList . fmap (\x -> (head x, length x)) . group . sort

deriv :: Integral n => [n] -> [n]
deriv (x:y:ys) = (y - x) : deriv (y:ys)
deriv _ = []

preprocess :: String -> [Int]
preprocess = (\xs -> (minimum xs - 1) : xs ++ [maximum xs + 3]) . sort . fmap (read @Int) . lines

trib :: Int -> Integer
trib = (tribs 0 1 1 !!)
  where tribs a b c = a : tribs b c (a + b + c)

part1 :: String -> String
part1 = show . go . frequencies . deriv . preprocess
  where go xs = product . fmap (\n -> M.findWithDefault n 0 xs) $ [1, 3]

part2 :: String -> String
part2 = show . product . fmap (trib . length) . splitOn [3] . deriv . preprocess
