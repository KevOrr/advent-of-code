#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 02)
-- 1202 Program Alarm

import Data.List.Split
import Text.Printf
import System.IO
import System.Environment
import System.Exit

expected_output :: Int
expected_output = 19690720

restore :: Int -> Int -> [Int] -> [Int]
restore noun verb (x:_:_:rest) = x:noun:verb:rest
restore _ _ _ = error "memory too malformed to be restored"

interp :: [Int] -> Int -> [Int]
interp mem ip =
  case drop ip mem of
    99:_ -> mem
    opcode : a : b : dest : _ ->
      let op = case opcode of
                 1 -> (+)
                 2 -> (*)
                 _ -> error "invalid opcode"
      in case splitAt dest mem of
           (before, _:after) -> interp (before ++ (op (mem !! a) (mem !! b)):after) (ip + 4)
           _ -> error "destination out of range"
    _ -> error "unexpected end of memory"

brute :: [Int] -> Int -> Int -> Int -> Maybe (Int, Int)
brute _ _ 100 _ = Nothing
brute mem expected noun 100 = brute mem expected (noun + 1) 0
brute mem expected noun verb
  | interp (restore noun verb mem) 0 !! 0 == expected = Just (noun, verb)
  | otherwise = brute mem expected noun (verb + 1)

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  mem <- (fmap (read::String -> Int) . splitOn ",") <$> hGetContents f
  printf "Interpret 1202: %d\n" $ interp (restore 12 02 mem) 0 !! 0
  printf "Searching for instruction that outputs %d...\n" expected_output
  case (brute mem expected_output 0 0) of
    Just (noun, verb) -> printf "Found instruction: %2d%2d\n" noun verb
    Nothing -> print "No instruction found"
