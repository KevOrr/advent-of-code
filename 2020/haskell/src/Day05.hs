module Day05 where

import Data.List (sort, foldl')
import qualified Data.Set as S

binToDec :: [Bool] -> Int
binToDec = foldl' (\a x -> 2*a + fromEnum x) 0

parseSeat :: String -> Int
parseSeat = binToDec . fmap (`elem` ['B', 'R'])

part1 :: String -> String
part1 = show . maximum . fmap parseSeat . lines

part2 :: String -> String
part2 = show . head . S.toList . (go <*> head <*> last) . sort . fmap parseSeat . lines
  where go xs min max = S.fromAscList [min..max] `S.difference` S.fromAscList xs
