module Day2 where

import qualified AoC.Lib as L

-- parse
toRound :: [b] -> (b, b)
toRound line = (them,me)
  where them = head line
        me = line !! 2
        
parse :: String -> [(Char, Char)]
parse = map toRound . lines

-- part1
resultValue :: Int -> Int -> Int
resultValue them me
  | result == 23 = 3 -- draw
  | result == 24 || result == 21 = 6 -- win
  | otherwise = 0
  where result = me - them

roundScore :: (Char, Char) -> Int
roundScore (them,me) = optionValue + resultValue themValue meValue
  where
    themValue = fromEnum them
    meValue = fromEnum me
    optionValue = meValue - 87 -- 'X' = ascii 88

part1 :: [(Char, Char)] -> Int
part1 = sum . map roundScore

-- part2
modChar :: Int -> Char -> Char -- small helper to do char arithmetic
modChar n = toEnum . (+n) . fromEnum

fixRound :: (Char, Char) -> (Char, Char)
fixRound (them, 'Y') = (them, modChar 23 them) -- draw
fixRound (them, 'X') -- need to lose
  | them > 'A'  = (them, modChar 22 them) -- b,c win to x,y resp
  | otherwise = (them, modChar 25 them) -- x defeats a
fixRound (them,'Z') -- need to win
  | them < 'C' = (them, modChar 24 them)-- a,b lose to y,z
  | otherwise = (them, modChar 21 them)-- c loses to x

part2 :: [(Char, Char)] -> Int
part2 = part1 .  map fixRound

-- solve
main :: IO ()
main = do
 let solution = L.Solution 2 parse part1 part2
 L.solve solution
