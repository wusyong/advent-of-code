module Main where
    
main :: IO ()
main = do
    inputs <- lines <$> readFile "../../input/01.txt"
    let nums = map read inputs :: [Int]
    let result = part1 nums
    print result
    let result = part2 nums
    print result

part1 :: (Eq a, Num a) => [a] -> a
part1 nums = head [a*b | a <- nums, b <- nums, a + b == 2020]

part2 :: (Eq a, Num a) => [a] -> a
part2 nums = head [a*b*c | a <- nums, b <- nums, c <- nums, a + b + c == 2020]
