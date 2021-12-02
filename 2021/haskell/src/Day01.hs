module Day01 where

parse :: String -> [Int]
parse = fmap read . lines

sliding3sum :: [Int] -> [Int]
sliding3sum = fmap (\(a, b, c) -> a + b + c) . (\ds -> zip3 ds (tail ds) (tail $ tail ds))

countIncreasing :: [Int] -> Int
countIncreasing = length . filter (uncurry (<)) . (\ds -> zip ds (tail ds))

part1 :: String -> String
part1 = show . countIncreasing . parse

part2 :: String -> String
part2 = show . countIncreasing . sliding3sum . parse
