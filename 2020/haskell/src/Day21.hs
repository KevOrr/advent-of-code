{-# LANGUAGE TupleSections #-}
module Day21 where

import Data.Foldable
import Control.Monad
import Control.Monad.Loops (whileJust)
import Control.Monad.State.Lazy
import Data.List
import Data.Ord
import Data.Void (Void)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Ingredient = Ingredient String
  deriving (Eq, Ord, Show)

newtype Alergen = Alergen String
  deriving (Eq, Ord, Show)

type Food = ([Ingredient], [Alergen])
type Mapping = M.Map Alergen (S.Set Ingredient)

parseFooden :: Parsec Void String [Food]
parseFooden = parseFood `endBy` (void eol <|> eof)
  where
    lexeme = L.lexeme hspace
    symbol = L.symbol hspace
    parseFood = do
      is <- some (lexeme (some letterChar))
      void (symbol "(" *> symbol "contains")
      as <- lexeme (some letterChar) `sepBy` symbol ","
      void (symbol ")")
      pure (Ingredient <$> is, Alergen <$> as)

possibleMappings :: [Food] -> Mapping
possibleMappings = foldl1 (M.unionWith S.intersection) . fmap (uncurry go)
  where go (S.fromList -> is) = M.fromList . fmap (,is)

solve1 :: Mapping -> (Maybe (Alergen, Ingredient), Mapping)
solve1 m = do
  let solvable = M.toList $ M.filter ((==1) . length) m
  (a, toList -> [i]) <- fst <$> uncons solvable
  let m' = M.map (S.filter (/= i)) m
  Just ((a, i), m')

solve :: Mapping -> ([(Alergen, Ingredient)], Mapping)
solve = state solve1
  where
    go xs m = case solve1 m of
      Just (x, m') -> go (xs ++ [x]) m'
      Nothing -> (xs, m)

main :: IO ()
main = do
  input <- readFile "src/f''"
  let fooden = either (error . errorBundlePretty) id . parse (parseFooden <* eof) "" $ input
      ingredientsList = concat (fst <$> fooden)
      solution = fst . solve $ possibleMappings fooden
      hypoalergens = S.fromList ingredientsList `S.difference` S.fromList (snd <$> solution)
      hypocount = length . filter (`S.member` hypoalergens) $ ingredientsList
  print hypocount
  print . intercalate "," . fmap (\(_, Ingredient i) -> i) . sortBy (comparing fst) $ solution
