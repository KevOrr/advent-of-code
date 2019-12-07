#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 05)
-- Sunny with a Chance of Asteroids

import Debug.Trace
import Data.List.Split
import Text.Printf
import System.IO
import System.Environment
import System.Exit

type Memory = [Int]
type Instr = [Int]
type Addr = Int

data Flag =
  Halted
  deriving (Eq, Show)

set_mem :: Addr -> Int -> Memory -> Memory
set_mem addr val mem = case splitAt addr mem of
  (before, _:after) -> before ++ val:after
  _ -> error "destination out of range"

fetchArg :: Int -> Int -> Memory -> Int
-- fetchArg mode arg _ | trace (printf "fetchArg %d %d mem" mode arg) False = undefined
fetchArg 0 arg mem = mem !! arg
fetchArg 1 arg _ = arg
fetchArg _ _ _ = error "invalid argmode"

fetchArgs :: [Int] -> [Int] -> Memory -> [Int]
-- fetchArgs modes args _ | trace (printf "fetchArgs %s %s mem" (show modes) (show args)) False = undefined
fetchArgs modes args mem = map (\(mode, arg) -> fetchArg mode arg mem) (zip modes args)

op2 :: (Int -> Int -> Int) -> [Int] -> [Int] -> Memory -> Int
op2 f modes args mem =
  case fetchArgs modes args mem of
    a:b:_ -> f a b
    _ -> error "Not enough arguments"

step :: [Int] -> Instr -> Memory -> Addr -> [Flag] -> (Handle, Handle) -> IO (Memory, Addr, [Flag])
-- step modes instr _ ip flags _ |
--   trace (printf "step %s %s mem %s %s _" (show modes) (show (take 8 instr)) (show ip) (show flags)) False = undefined
step _ (99:_) mem ip flags _ = return (mem, (ip + 1), (Halted:flags))
step modes (1:a:b:dest:_) mem ip flags _ = return (set_mem dest (op2 (+) modes [a, b] mem) mem, ip + 4, flags)
step modes (2:a:b:dest:_) mem ip flags _ = return (set_mem dest (op2 (*) modes [a, b] mem) mem, ip + 4, flags)
step _ (3:dest:_) mem ip flags (input, _) = hGetLine input >>= \l -> return (set_mem dest (read l) mem, ip + 2, flags)
step (mode:_) (4:a:_) mem ip flags (_, output) = hPrint output (fetchArg mode a mem) >> return (mem, ip + 2, flags)
step modes (5:a:b:_) mem ip flags _ =
  let [cond, dest] = fetchArgs modes [a, b] mem in
    return (mem, if cond /= 0 then dest else ip + 3, flags)
step modes (6:a:b:_) mem ip flags _ =
  let [cond, dest] = fetchArgs modes [a, b] mem in
    return (mem, if cond == 0 then dest else ip + 3, flags)
step modes (7:a:b:dest:_) mem ip flags _ = return (set_mem dest (op2 (\x y -> if x < y then 1 else 0) modes [a, b] mem) mem, ip + 4, flags)
step modes (8:a:b:dest:_) mem ip flags _ = return (set_mem dest (op2 (\x y -> if x == y then 1 else 0) modes [a, b] mem) mem, ip + 4, flags)
step _ _ _ _ _ _ = error "invalid instruction"

parseModes :: Int -> [Int]
parseModes 0 = [0, 0, 0]
parseModes x = (mod x 10):(parseModes (div x 10))

parseInstr :: Instr -> Instr
parseInstr [] = []
parseInstr (opcode:rest) = (mod opcode 100):rest

interp :: Memory -> Addr -> [Flag] -> (Handle, Handle) -> IO Memory
interp mem ip flags handles
  | elem Halted flags = return mem
  | otherwise = do
     (mem', ip', flags') <- step (parseModes (div (mem !! ip) 100)) (parseInstr (drop ip mem)) mem ip flags handles
     interp mem' ip' flags' handles

main :: IO Memory
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  mem <- (fmap (read::String -> Int) . splitOn ",") <$> hGetContents f
  interp mem 0 [] (stdin, stdout)
