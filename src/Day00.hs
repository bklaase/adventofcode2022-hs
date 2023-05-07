module Day00 where

import qualified AoC.Lib as L

-- parse
parse = id

-- part1
part1 = length

-- part2
part2 = length

-- solve
main :: IO ()
main = do
  let solution = L.Solution 00 parse part1 part2
  L.solve solution
