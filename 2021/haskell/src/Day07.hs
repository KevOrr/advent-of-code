module Day07 where

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.HashSet as HS

part1 :: String -> String
part1 = show . id . fmap (read @Int) . lines

part2 :: String -> String
part2 = show . id . fmap (read @Int) . lines
