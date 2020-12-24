module Day11 where

import Control.Monad
import Data.Maybe (catMaybes, fromJust)
import Data.List
import qualified Data.Map.Strict as M
import qualified System.Console.ANSI as ANSI
import qualified Control.DeepSeq as DS
import Control.Concurrent

data Plane a = Plane a [a] [a] [[a]] [[a]]
  deriving (Eq, Show)

iterUntil :: (a -> Bool) -> (a -> Maybe a) -> a -> Maybe a
iterUntil pred f x
  | Just x' <- f x = if pred x' then Just x' else iterUntil pred f x'
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
step f n = fromJust . listToPlane . go
  where
    go p = go1 p : maybe [] go (down p)
    go1 p = stepSeat f n p : maybe [] go1 (right p)

run :: (Plane Char -> [Char]) -> Int -> Plane Char -> [Plane Char]
run f n x = if x == x' then [x] else x : run f n x'
  where x' = step f n x

runner :: (Plane Char -> [Char]) -> Int -> String -> IO ()
runner f n s = do
  ANSI.clearScreen
  ANSI.setSGR []
  ANSI.hideCursor
  let planes = run f n . fromJust . listToPlane . lines $ s
  forM_ planes $ \(DS.force . planeToList -> p) -> do
    ANSI.setCursorPosition 0 0
    forM_ p $ \row -> do
      forM_ row $ \seat -> do
        case seat of
          'L' -> ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green,
                              ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Green]
          '#' -> ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red,
                              ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Red]
          '.' -> ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black,
                              ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Black]
          _ -> pure ()
        putChar seat
      ANSI.cursorDown 1
      ANSI.setCursorColumn 0
  ANSI.setSGR []
  ANSI.showCursor
  print . M.findWithDefault 0 '#' . frequencies . concat . planeToList . last $ planes


part1 :: String -> IO ()
part1 = runner adjacent 4

part2 :: String -> IO ()
part2 = runner (raycast "L#") 5
