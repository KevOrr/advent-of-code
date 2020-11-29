#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 06)
-- Universal Orbit Map

import Data.Maybe
import Control.Monad
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import qualified Data.MultiMap as MM
import Data.List
import Data.List.Split

type System = MM.MultiMap String String

merge2 :: Ord k => MM.MultiMap k v -> MM.MultiMap k v -> MM.MultiMap k v
merge2 m = MM.foldlWithKey (\n k v -> MM.insert k v n) m

merge :: Ord k => [MM.MultiMap k v] -> MM.MultiMap k v
merge = foldl merge2 MM.empty

readSystems :: String -> System
readSystems = \s -> merge (map readSystem (lines s))
  where readSystem s = let (a:b:_) = splitOn ")" s in MM.fromList [(a, b)]


totalOrbits :: System -> Integer
totalOrbits s = subtreeOrbits "COM"
  where
    transitiveOrbits body = (toInteger $ length $ satellites) + (sum $ map transitiveOrbits $ satellites)
      where satellites = MM.lookup body s
    subtreeOrbits body = transitiveOrbits body + (sum $ map subtreeOrbits $ MM.lookup body s)

path :: System -> String -> String -> Maybe [String]
path s from to
  | from == to = Just [from]
  | otherwise =
      case
        do one_path <- map (\child -> path s child to) (MM.lookup from s) :: [Maybe [String]]
           case liftM (from:) one_path of
             Just the_path -> return the_path
             _ -> []
      of
        x:_ -> Just x
        [] -> Nothing

joinPaths :: [String] -> [String] -> Maybe String
joinPaths (x:xx:xs) (y:yy:ys)
  | x == y && xx == yy = joinPaths (xx:xs) (yy:ys)
  | x == y = Just x
  | otherwise = Nothing
joinPaths _ _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  system <- (readSystems) <$> hGetContents f
  printf "Total direct and indirect orbits in this system: %d\n" (totalOrbits system)
  let n = do path_san <- (path system "COM" "SAN")
             path_you <- (path system "COM" "YOU")
             common <- joinPaths path_san path_you
             common_idx <- elemIndex common path_san
             -- return $ (length path_san - 1 - common_idx - 1) + (length path_you - 1 - common_idx - 1)
             return $ length path_san + length path_you - 2 * common_idx - 4
  case n of
    Just n -> printf "Shortest path between YOU and SAN: %d\n" n
    Nothing -> print "No path found between YOU and SAN"
