module Day09 where

import Data.List (tails)
import qualified Data.Set as S
import Debug.Trace (trace)

extend :: Int -> S.Set Int -> S.Set Int
extend n s = S.map (n+) s `S.union` s

hasPair :: Int -> S.Set Int -> Bool
hasPair n s = S.foldr (\m b -> b || (n - m) `S.member` s) False s

valid :: [Int] -> Int -> Either () Int
valid prev x
  | hasPair x (S.fromList prev) = Left ()
  | otherwise = Right x

find :: [Int] -> Int
find = go . fmap (take 26) . tails
  where
    go [] = 0
    go (xs:xss) =
      case valid (take 25 xs) (last xs) of
        Left _ -> go xss
        Right n -> n

findThing :: [Int] -> Int
findThing = go [] 0
  where
    go [] 0 (x:xs) = go [x] x xs
    go (y:ys) running xs
      | running == 1398413738 = minimum (y:ys) + maximum (y:ys)
      | running > 1398413738 = go ys (running - y) xs
      | x:xs' <- xs = go (y:ys ++ [x]) (running + x) xs'
      | otherwise = error "empty"
    go [] _ [] = error "empty"

part1 :: String -> String
part1 = show . find . fmap read . lines

part2 :: String -> String
part2 = show . findThing . fmap read . lines
