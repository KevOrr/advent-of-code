{-# LANGUAGE TypeApplications #-}

module Day01
    ( run1
    ) where

import Data.Maybe (mapMaybe)
import Text.Read

stableDigits :: Integral n => n -> [n] -> [n]
stableDigits n (x1:x2:xs)
  | x1 == x2 = x1 : stableDigits n (x2:xs)
  | otherwise = stableDigits n (x2:xs)
stableDigits n [x] = if n == x then [n] else []
stableDigits _ [] = []

readCharsIgnoringErrors :: (Read a) => String -> [a]
readCharsIgnoringErrors = mapMaybe (readMaybe . pure)

run1 :: String -> String
run1 s =
  case readCharsIgnoringErrors @Int s of
    (n:ns) -> show . sum $ stableDigits n (n:ns)
    _ -> error "Empty or non-numeric input"
