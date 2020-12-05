module Day03 where

readForest :: String -> [[Bool]]
readForest s = fmap (== '#') . cycle <$> lines s

diag :: Int -> Int -> [[a]] -> [a]
diag _ _ [] = []
diag right down xs = head (head xs) : diag right down (drop right <$> drop down xs)

part1 :: String -> String
part1 = show . length . filter id . diag 3 1 . readForest

part2 :: String -> String
part2 s = show . product $ ($ readForest s) <$> fs
  where
    fs = (\(r, d) -> length . filter id . diag r d) <$> slopes
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
