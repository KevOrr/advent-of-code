#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- Advent of Code 2019 (day 03)
-- Crossed Wires

{-# LANGUAGE OverloadedStrings #-}

import System.IO
import qualified Data.Text as Text
import Text.Printf
import System.Environment
import System.Exit
import qualified Data.Map
import Data.List hiding (intersect)
import Data.Maybe()
import Data.Functor.Classes

type Point = (Int, Int)
data Line =
    Linev Point Int
  | Lineh Point Int
  deriving Show

begin :: Line -> Point
begin (Linev p _) = p
begin (Lineh p _) = p

end :: Line -> Point
end (Linev (x, y) h) = (x, y + h)
end (Lineh (x, y) w) = (x + w, y)

len :: Line -> Int
len (Linev _ h) = abs h
len (Lineh _ w) = abs w

intersect :: Line -> Line -> Maybe Point
intersect (Linev _ _) (Linev _ _) = Nothing
intersect (Lineh _ _) (Lineh _ _) = Nothing
intersect (Linev p1 h) (Lineh p2 w) = intersect (Lineh p2 w) (Linev p1 h)
intersect (Lineh (x1, y1) w) (Linev (x2, y2) h)
  | (min x1 (x1 + w)) <= x2 && x2 <= (max x1 (x1 + w)) && (min y2 (y2 + h)) <= y1 && y1 <= (max y2 (y2 + h)) = Just (x2, y1)
  | otherwise = Nothing

getIntersects = getIntersectsExcept []

getIntersectsExcept except xs ys = getIntersectsExcept' except xs xs ys

getIntersectsExcept' :: [Point] -> [Line] -> [Line] -> [Line] -> [Point]
getIntersectsExcept' _ _ _ [] = []
getIntersectsExcept' except xs [] (_:ys) = getIntersectsExcept' except xs xs ys
getIntersectsExcept' except allxs (x:xs) (y:ys) =
  case intersect x y of
    Just p
      | elem p except -> getIntersectsExcept' except allxs xs (y:ys)
      | otherwise -> p:getIntersectsExcept' except allxs xs (y:ys)
    Nothing -> getIntersectsExcept' except allxs xs (y:ys)

wire :: Point -> [Text.Text] -> [Line]
wire _ [] = []
wire p (move:moves) =
  line:(wire (end line) moves)
  where dir = Text.head move
        mag = (read . Text.unpack) (Text.tail move)
        line = case dir of
                 _ | dir == 'R' -> Lineh p mag
                   | dir == 'L' -> Lineh p (-mag)
                   | dir == 'U' -> Linev p mag
                   | dir == 'D' -> Linev p (-mag)
                   | otherwise -> error $ printf "Not a valid direction: %s" dir

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findClosest :: Point -> [Point] -> Maybe (Point, Int)
findClosest _ [] = Nothing
findClosest p (q:qs) =
  case findClosest p qs of
    Nothing -> Just (q, manhattan p q)
    Just (r, d)
      | d' < d -> Just (q, d')
      | otherwise -> Just (r, d)
      where d' = manhattan p q

pointsOn :: Line -> [Point]
pointsOn (Lineh (x, y) w)
  | w == 0 = [(x, y)]
  | w < 0 = (x, y):(pointsOn (Lineh (x - 1, y) (w + 1)))
  | w > 0 = (x, y):(pointsOn (Lineh (x + 1, y) (w - 1)))
pointsOn (Linev (x, y) h)
  | h == 0 = [(x, y)]
  | h < 0 = (x, y):(pointsOn (Linev (x, y - 1) (h + 1)))
  | h > 0 = (x, y):(pointsOn (Linev (x, y + 1) (h - 1)))

distmap :: [Line] -> Data.Map.Map Point Int
distmap wire =
  Data.Map.fromList (getWireDists 0 wire)
  where getLineDists _ [] = []
        getLineDists d (x:xs) = (x, d):(getLineDists (d + 1) xs)
        getWireDists _ [] = []
        getWireDists d (l:ls) = getLineDists d (pointsOn l) ++ getWireDists (d + len l) ls

findShortest :: [Line] -> [Line] -> [Point] -> Maybe (Point, Int)
findShortest l1 l2 ps =
  minimumBy (liftCompare (\(_, pdist) (_, qdist) -> compare pdist qdist)) $
  map (\p -> do
           d1 <- Data.Map.lookup p dm1
           d2 <- Data.Map.lookup p dm2
           return (p, d1 + d2)) ps
  where dm1 = distmap l1
        dm2 = distmap l2

main :: IO ()
main = do
  args <- getArgs
  f <- case args of
         [] -> return stdin
         [f] -> openFile f ReadMode
         _ -> do prog <- getProgName; hPutStrLn stderr (printf "usage: %s [FILE]" prog); exitWith (ExitFailure 1)
  l1 <- hGetLine f
  l2 <- hGetLine f
  let wire1 = wire (0, 0) $ Text.splitOn "," (Text.pack l1)
  let wire2 = wire (0, 0) $ Text.splitOn "," (Text.pack l2)
  let intersects = getIntersectsExcept [(0, 0)] wire1 wire2
  printf "Found intersections: %s\n" (show intersects)
  let closest = findClosest (0, 0) intersects
  case closest of
    Nothing -> print "No intersections!"
    Just (p, d) -> printf "Closest intersection to (0, 0): %s with distance %d\n" (show p) d
  let shortest = findShortest wire1 wire2 intersects
  case shortest of
    Nothing -> print "No intersections!"
    Just (p, d) -> printf "Intersection with shortest wire lengths: %s with distance %d\n" (show p) d
