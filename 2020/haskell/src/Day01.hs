module Day01 where

import qualified Data.Set as S
import Data.List (sort)
import Control.Monad (join)

sum2 :: Integral n => n -> [n] -> Maybe [n]
sum2 target = go <*> S.fromList
  where
    go (x:xs) s
      | (target - x) `S.member` s = Just [x, target - x]
      | otherwise = go xs s
    go [] _ = Nothing

sumn :: Integral n => n -> n -> [n] -> Maybe [n]
sumn 2 target = sum2 target
sumn n target = join go
  where
    go (x:xs) xs' =
      case sumn (n - 1) (target - x) xs' of
        Just ys -> Just (x:ys)
        Nothing -> go xs xs'
    go [] _ = Nothing

part1 :: String -> String
part1 = maybe "No sum found" (show . product) . sum2 2020 . fmap read . lines

part2 :: String -> String
part2 = maybe "No sum found" (show . product) . sumn 3 2020 . fmap read . lines
