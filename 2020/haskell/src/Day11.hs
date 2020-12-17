module Day11 where

import Control.Monad ((>=>))
import Data.Maybe (catMaybes, fromJust)
import Data.List
import qualified Data.Map.Strict as M
import System.IO

data Plane a = Plane a [a] [a] [[a]] [[a]]
  deriving (Eq, Show)

iterUntil :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
iterUntil pred f x
  | Just x' <- f x = if pred x' then Just x else iterUntil pred f x'
  | otherwise = Nothing

listToPlane :: [[a]] -> Maybe (Plane a)
listToPlane ((x : rs) : bs) = Just $ Plane x [] rs [] bs
listToPlane _ = Nothing

planeToList :: Plane a -> [[a]]
planeToList (Plane x ls rs ts bs) = reverse ts ++ [reverse ls ++ [x] ++ rs] ++ bs

getCur :: Plane a -> a
getCur (Plane x _ _ _ _) = x

left :: Plane a -> Maybe (Plane a)
left (Plane _ [] _ _ _) = Nothing
left (Plane cur (l:ls) rs ts bs) = Just $ Plane l ls (cur:rs) ts bs

right :: Plane a -> Maybe (Plane a)
right (Plane _ _ [] _ _) = Nothing
right (Plane cur ls (r:rs) ts bs) = Just $ Plane r (cur:ls) rs ts bs

up :: Plane a -> Maybe (Plane a)
up (Plane _ _ _ [] _) = Nothing
up (Plane cur ls rs (t:ts) bs) = Just $ Plane cur' ls' rs' ts (b:bs)
  where
    (ls', cur':rs') = splitAt (length ls) t
    b = ls ++ cur : rs

down :: Plane a -> Maybe (Plane a)
down (Plane _ _ _ _ []) = Nothing
down (Plane cur ls rs ts (b:bs)) = Just $ Plane cur' ls' rs' (t:ts) bs
  where
    (ls', cur':rs') = splitAt (length ls) b
    t = ls ++ cur : rs


cardinals :: [Plane a -> Maybe (Plane a)]
cardinals = [left, right, up, down, left >=> up, right >=> up, left >=> down, right >=> down]

adjacent :: Plane a -> [a]
adjacent p = getCur <$> catMaybes (($ p) <$> cardinals)

raycast :: Eq a => [a] -> Plane a -> [a]
raycast stops p = getCur <$> catMaybes ((\f -> iterUntil ((`elem` stops) . getCur) f p) <$> cardinals)

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = M.fromList . fmap (\x -> (head x, length x)) . group . sort

seatFills :: (Plane Char -> [Char]) -> Plane Char -> Bool
seatFills f p = getCur p == 'L' && M.findWithDefault 0 '#' (frequencies $ f p) == 0

seatEmpties :: (Plane Char -> [Char]) -> Int -> Plane Char -> Bool
seatEmpties f n p = getCur p == '#' && M.findWithDefault 0 '#' (frequencies $ f p) >= n

stepSeat :: (Plane Char -> [Char]) -> Int -> Plane Char -> Char
stepSeat f n p
  | seatFills f p = '#'
  | seatEmpties f n p = 'L'
  | otherwise = getCur p

step :: (Plane Char -> [Char]) -> Int -> Plane Char -> Plane Char
step f n x = fromJust . listToPlane . go $ x
  where
    go p = go1 p : maybe [] go (down p)
    go1 p = stepSeat f n p : maybe [] go1 (right p)

run :: (Plane Char -> [Char]) -> Int -> Plane Char -> Plane Char
run f n x = if x == x' then x else run f n x'
  where x' = step f n x

part1 :: String -> String
part1 = show . M.findWithDefault 0 '#' . frequencies . concat . planeToList . run adjacent 4 . fromJust . listToPlane . lines

part2 :: String -> String
part2 = show . M.findWithDefault 0 '#' . frequencies . concat . planeToList . run (raycast "L#") 5 . fromJust . listToPlane . lines
