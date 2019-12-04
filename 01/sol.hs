#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 01)
-- The Tyranny of the Rocket Equation

import System.IO
import System.Environment
import System.Exit
import Text.Printf

fuel_simpl :: Integer -> Integer
fuel_simpl x = (x `div` 3) - 2

fuel_recur :: Integer -> Integer
fuel_recur x =
  let fuel = ((x `div` 3) - 2) in
    if fuel <= 0 then 0 else fuel + fuel_recur fuel

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  mods <- (map (read::String -> Integer) . lines) <$> hGetContents f;
  (printf "Challenge 1: %d\n" . sum . map fuel_simpl) mods;
  (printf "Challenge 2: %d\n" . sum . map fuel_recur) mods
