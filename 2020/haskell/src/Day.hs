module Day
  ( Day()
  , mkDay
  , Part(..)
  , Parseable(..)
  , SolutionRunner(..)
  , Solution(..)
  , runSolution
  ) where

import Data.Functor.Identity

newtype Day = Day Int

mkDay :: Int -> Day
mkDay n
  | n < 1 = error "day must be >= 1"
  | n > 25 = error "day must be <= 25"
  | otherwise = Day n

data Part = One | Two

class Parseable e a where
  parser :: String -> Either e a

class SolutionRunner f a where
  run :: (a -> String) -> f a -> String

instance (Parseable e a, Show e) => SolutionRunner (Either e) a where
  run = either show

instance SolutionRunner Identity a where
  run f = f . runIdentity

data Solution f a =
  Solution
    { solutionParser :: String -> f a
    , solutionPart1 :: a -> String
    , solutionPart2 :: a -> String
    , solutionTests1 :: [(a, String)]
    , solutionTests2 :: [(a, String)]
    }

runSolution :: SolutionRunner f a => Solution f a -> Part -> String -> String
runSolution s One = run (solutionPart1 s) . solutionParser s
runSolution s Two = run (solutionPart2 s) . solutionParser s
