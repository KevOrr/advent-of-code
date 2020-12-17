module Day16 where

import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import qualified Data.Set as S

type Spec = (String, [(Int, Int)])
type Ticket = [Int]

parse' :: String -> ([Spec] , Ticket , [Ticket])
parse' s =
  let [specs, [_, your], _:their] = splitOn [""] $ lines s in
    (parseSpec <$> specs, parseTicket your, parseTicket <$> their)
  where
    parseSpec (splitOn ": " -> [name, bounds]) =
      (name, (\[a, b] -> (read a, read b)) . splitOn "-" <$> splitOn " or " bounds)
    parseTicket s = read <$> splitOn "," s

valid :: Spec -> Int -> Bool
valid s x = any (\(low, high) -> low <= x && x <= high) $ snd s

possibleMappings :: [Spec] -> [Ticket] -> S.Set (Int, Int)
possibleMappings ss ts =
  foldl1 S.intersection $
  [ S.fromList [ (si, xi)
               | (si, s) <- zip [0..] ss
               , (xi, x) <- zip [0..] t
               , valid s x
               ]
  | t <- ts
  ]

invalid :: [Spec] -> Ticket -> [Int]
invalid s = filter (\x -> not $ any (`valid` x) s)

part1 :: String -> String
part1 s =
  let (specs, _, their) = parse' s in
    show . sum . concatMap (invalid specs) $ their

part2 :: String -> String
part2 s =
  let (specs, _, their) = parse' s in
    show . possibleMappings specs . filter (all (\x -> any (`valid` x) specs)) $ their

main :: IO ()
main = readFile "src/f" >>= putStrLn . part2
