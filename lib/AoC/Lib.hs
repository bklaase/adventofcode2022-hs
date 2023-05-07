module AoC.Lib where

import Data.Time.Clock ( NominalDiffTime, diffUTCTime, getCurrentTime )
import System.IO ( IOMode(ReadMode), hGetContents, openFile )
import Text.Printf (printf)

data Solution  a b c = Solution
  { day :: Int,
    parse :: String -> a,
    part1 :: a -> b,
    part2 :: a -> c }

data BenchmarkResult a = BenchmarkResult
  { answer :: a,
    runTime :: NominalDiffTime } deriving Show

solve :: (Show b, Show c) => Solution a b c -> IO ()
solve solution = do
  handle <- openFile  (printf "./input/day%d.txt"
                      (day solution))  ReadMode
  inputStr <- hGetContents handle
  let input = parse solution inputStr
  ans1 <- benchmark $ part1 solution input
  putStrLn $ "Part1: " ++ show ans1
  ans2 <- benchmark $ part2 solution input
  putStrLn $ "Part2: " ++ show ans2


benchmark :: a -> IO (BenchmarkResult a)
benchmark a = do
  startTime <- getCurrentTime
  a `seq` return ()
  stopTime <- getCurrentTime
  let runTime = diffUTCTime  stopTime startTime
  return $ BenchmarkResult a runTime
