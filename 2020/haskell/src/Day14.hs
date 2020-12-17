module Day14 where

import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map.Strict as M
import Data.Bits

data Bit = One | Zero | X

type Mask = [(Int, Bit)]
type Write = (Int, Integer)
type Program = [(Mask, [Write])]
type Mem = M.Map Int Integer

parser :: Parsec Void String Program
parser = many parseOne
  where
    parseOne = (,) <$> parseMask <*> (parseWrite `endBy` many (char '\n'))
    parseMask = string "mask = " *> (convertMask <$> some (oneOf ['X', '0', '1'])) <* many (char '\n')
    parseWrite =
      (,)
      <$> (string "mem[" *> (read <$> some digitChar))
      <*> (string "] = " *> (read <$> some digitChar))
    convertMask = fmap (fmap convertBit) . zip [0..] . reverse
    convertBit '1' = One
    convertBit '0' = Zero
    convertBit _ = X

mask :: Bits n => Mask -> n -> n
mask xs n = foldl' mask1 n xs
  where
    mask1 n (i, One) = setBit n i
    mask1 n (i, Zero) = clearBit n i
    mask1 n (_, X) = n

interpWithMask1 :: Mask -> [Write] -> Mem -> Mem
interpWithMask1 _ [] mem = mem
interpWithMask1 m ((addr, val):ws) mem = interpWithMask1 m ws $ M.insert addr (mask m val) mem

addrSet :: Bits n => Mask -> n -> [n]
addrSet [] n = [n]
addrSet ((i, One):ms) n = addrSet ms $ setBit n i
addrSet ((_, Zero):ms) n = addrSet ms n
addrSet ((i, X):ms) n = do
  other <- addrSet ms n
  [setBit other i, clearBit other i]

interpWithMask2 :: Mask -> [Write] -> Mem -> Mem
interpWithMask2 _ [] mem = mem
interpWithMask2 m ((addr, val):ws) mem = interpWithMask2 m ws $ foldr (`M.insert` val) mem (addrSet m addr)

interp :: (Mask -> [Write] -> Mem -> Mem) -> Program -> Mem -> Mem
interp f xs mem = foldl' (\mem (m, ws) -> f m ws mem) mem xs

part1 :: String -> String
part1 = either errorBundlePretty (show . sum . M.elems . flip (interp interpWithMask1) M.empty) . parse parser ""

part2 :: String -> String
part2 = either errorBundlePretty (show . sum . M.elems . flip (interp interpWithMask2) M.empty) . parse parser ""

main :: IO ()
main = readFile "src/f" >>= putStrLn . part2
