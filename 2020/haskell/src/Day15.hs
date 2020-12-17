module Day15 where

import Data.List.Split (splitOn)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)

type Number = Int
type Position = Int
type History s = STUArray s Number Position

interp :: Position -> [Number] -> Number
interp target xs = runST do
  hist <- newArray (0, target) 0
  forM_ (zip xs [1..]) (uncurry $ writeArray hist)
  go (length xs) (last xs) hist
  where
    go :: Position -> Number -> History s -> ST s Number
    go pos prev hist =
      if pos > target then
        pure prev
      else do
        pos' <- readArray hist prev
        writeArray hist prev pos
        go (pos + 1) (if pos' == 0 then 0 else pos - pos') hist

part1 :: String -> String
part1 = show . interp (2020 - 1) . fmap read . splitOn ","

part2 :: String -> String
part2 = show . interp (30000000 - 1) . fmap read . splitOn ","
