module Day12 where

import Data.Maybe (fromJust)
import Text.Parsec (Parsec, parse, try)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec.Char (digit, oneOf, char)

data Dir = N | E | S | W
  deriving (Enum, Eq, Ord, Show)

data RotDir = L | R
  deriving (Enum, Eq, Ord, Show)

data Cmd = GoDir Dir Int | Rot RotDir Int | F Int
  deriving (Eq, Show)

type Pos = (Int, Int)

data Turtle =
  Turtle
  { turtleDir :: Dir
  , turtleLoc :: Pos
  }
  deriving (Eq, Show)

parseCmd :: Parsec String () Cmd
parseCmd =
  fromJust . flip lookup cmds
  <$> oneOf "NSEWLRF"
  <*> (read <$> many1 digit)
  where
    cmds =
      [ ('N', GoDir N)
      , ('S', GoDir S)
      , ('E', GoDir E)
      , ('W', GoDir W)
      , ('L', Rot L)
      , ('R', Rot R)
      , ('F', F)
      ]

parseCmds :: String -> [Cmd]
parseCmds = either (error . show) id . parse (parseCmd `sepBy` (try . many1 $ char '\n')) ""

rotateDir :: RotDir -> Dir -> Dir
rotateDir L N = W
rotateDir L E = N
rotateDir L S = E
rotateDir L W = S
rotateDir R S = W
rotateDir R N = E
rotateDir R E = S
rotateDir R W = N

translate :: Dir -> Int -> Pos -> Pos
translate N n (x, y) = (x, y+n)
translate E n (x, y) = (x+n, y)
translate S n (x, y) = (x, y-n)
translate W n (x, y) = (x-n, y)

rotate :: RotDir -> Pos -> Pos
rotate L (x, y) = (-y, x)
rotate R (x, y) = (y, -x)

manhattan :: Num n => (n, n) -> (n, n) -> n
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

compose :: Foldable t => t (a -> a) -> a -> a
compose = foldl (flip (.)) id

part1 :: String -> String
part1 = show . manhattan (0, 0) . turtleLoc . ($ Turtle E (0,0)) . compose . fmap step . parseCmds
  where
    step :: Cmd -> Turtle -> Turtle
    step (GoDir gd n) (Turtle td p) = Turtle td $ translate gd n p
    step (Rot r n) (Turtle d (x, y)) = Turtle (iterate (rotateDir r) d !! (n `div` 90)) (x, y)
    step (F n) (Turtle d p) = Turtle N $ translate d n p

part2 :: String -> String
part2 = show . manhattan (0, 0) . turtleLoc . snd . ($ ((10, 1), Turtle E (0,0))) . compose . fmap step . parseCmds
  where
    step :: Cmd -> (Pos, Turtle) -> (Pos, Turtle)
    step (GoDir d n) (w, t) = (translate d n w, t)
    step (Rot r n) (w, Turtle _ s) = (iterate (rotate r) w !! (n `div` 90), Turtle E s)
    step (F n) ((dx, dy), Turtle _ (x, y)) = ((dx, dy), Turtle E (x+dx*n, y+dy*n))

main :: IO ()
main = readFile "src/f" >>= putStrLn . part2
