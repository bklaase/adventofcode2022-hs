module Day3 where

import qualified AoC.Lib as L
import Data.List (intersect)
import Data.Char (isAsciiLower, isAsciiUpper)

-- parse
parse :: String -> [String]
parse = lines

-- part1
findDuplicate :: String -> Char
findDuplicate =  head . getDupes . halve --asume only one duplicate
  where getDupes(x, y) = x `intersect` y
        halve x = splitAt (floor (fromIntegral (length x)/2)) x

itemPrio :: Char -> Int
itemPrio c
 | isAsciiLower c = fromEnum c - 96
 | isAsciiUpper c = fromEnum c - 38

part1 :: [String] -> Int
part1 = sum . map (itemPrio . findDuplicate)

-- part2
findBadge :: String -> String -> String -> Char
findBadge x y z =  head $ (x `intersect` y) `intersect` z

badges :: [String] -> String
badges (x:y:z:rest) = findBadge x y z : badges rest
badges rest = []

part2 :: [String] -> Int
part2  = sum . map itemPrio . badges

-- solve
main :: IO ()
main = do
  let solution = L.Solution 3 parse part1 part2
  L.solve solution
