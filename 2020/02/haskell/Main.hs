module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

type Line = (Int, Int, Char, String)

main :: IO ()
main = do
  input <- fmap Text.lines (Text.readFile "../../input/02.txt")
  let lines = map Text.unpack input
  let log = map (map getLog) lines
  let split = map words log
  let parsed = map parse split
  print $ length $ filter part1 parsed
  print $ length $ filter part2 parsed

getLog :: Char -> Char
getLog '-' = ' '
getLog ':' = ' '
getLog c = c

parse :: [String] -> Line
parse (a : b : c : d : _) = (read a, read b, head c, d)

part1 :: Line -> Bool
part1 (min, max, char, string) = (count >= min) && (count <= max)
  where
    count = length $ filter (== char) string

part2 :: Line -> Bool
part2 (min, max, char, string) = (fst == char) /= (snd == char)
  where
    fst = string !! (min - 1)
    snd = string !! (max - 1)
