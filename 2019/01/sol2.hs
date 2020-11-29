#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 01)
-- The Tyranny of the Rocket Equation

import System.IO
import System.Environment
import System.Exit
import Text.Printf

why_fuel_recur :: (Integer -> Integer) -> (Integer -> Integer)
why_fuel_recur f x =
  let fuel = ((x `div` 3) - 2) in
    if fuel <= 0 then 0 else fuel + f fuel

fix :: (a -> a) -> a
fix f = f (fix f)

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  mods <- (map (read::String -> Integer) . lines) <$> hGetContents f;
  (printf "Challenge 1: %d\n" . sum . map (why_fuel_recur (\_ -> 0))) mods;
  (printf "Challenge 2: %d\n" . sum . map (fix why_fuel_recur)) mods
