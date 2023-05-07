module Day1 where

import qualified AoC.Lib as L
import Data.List ( sortOn )

splitWhen :: (a-> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p l@(x:xs)
  | p x = splitWhen p xs
  | otherwise = group : splitWhen p rest
      where (group, rest) = break p l

parse :: String -> [[Integer]]
parse = map (map read) . splitWhen (== "") . lines

part1 :: [[Integer]] -> Integer
part1 = maximum . map sum

part2 :: [[Integer]] -> Integer
part2 = sum . take 3 . sortOn negate . map sum

main :: IO ()
main = do
  let solution = L.Solution 1 parse part1 part2
  L.solve solution
