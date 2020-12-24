module Day23 where

import Data.Function (fix)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Data.Foldable (traverse_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)

type Pointers s = STUArray s Int Int

makePointersArray :: Int -> [Int] -> forall s. ST s (Pointers s)
makePointersArray maxVal xs = do
  let xs' = xs ++ [maximum xs + 1 .. maxVal]
  pointers <- newArray (1, maxVal) 0
  traverse_ (uncurry $ writeArray pointers) $ zip xs' (tail xs' ++ [head xs'])
  pure pointers

findTarget :: Int -> [Int] -> Int -> Int
findTarget maxVal taken cand
  | cand > 0 && cand `notElem` taken = cand
  | cand > 1 = findTarget maxVal taken (cand - 1)
  | otherwise = findTarget maxVal taken maxVal

interp1 :: Int -> Int -> forall s. Pointers s -> ST s ()
interp1 cur maxVal arr = do
  x <- readArray arr cur
  y <- readArray arr x
  z <- readArray arr y
  next <- readArray arr z
  let target = findTarget maxVal [x, y, z] (cur - 1)
  targetNext <- readArray arr target
  writeArray arr target x
  writeArray arr z targetNext
  writeArray arr cur next

interp :: Int -> Int -> Int -> Int -> [Int] -> [Int]
interp n maxVal chainStart chainCount xs = runST do
  arr <- makePointersArray maxVal xs
  fix (\loop cur count ->
          if count > 0 then do
            interp1 cur maxVal arr
            next <- readArray arr cur
            loop next (count - 1)
          else
            pure ())
    (head xs) n
  fix (\loop cur count ->
          if count > 0 then do
            next <- readArray arr cur
            (cur :) <$> loop next (count - 1)
          else
            pure [])
    chainStart chainCount

main :: IO ()
main = do
  input <- mapMaybe (readMaybe @Int . pure) <$> readFile "day23.txt"
  let sol1 = interp 100 9 1 9 input
  putStrLn . tail . concatMap show $ sol1
  let sol2 = interp (10 ^ 7) (10 ^ 6) 1 3 input
  print . product . tail $ sol2
