{-# LANGUAGE FlexibleContexts, LambdaCase #-}

import Control.Monad (foldM_, zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (Ix, MArray, STUArray, getBounds, newArray_, readArray, writeArray)
import Data.List ((\\))

main :: IO ()
main = do
    let result = part1 [5, 3, 8, 9, 1, 4, 7, 6, 2]
    print result
    let result = part2 [5, 3, 8, 9, 1, 4, 7, 6, 2]
    print result

step :: (MArray a e m, Enum e, Ix e) => a e e -> e -> m e
step arr x = do
    (lo, hi) <- getBounds arr
    a <- readArray arr x
    b <- readArray arr a
    c <- readArray arr b
    y <- readArray arr c
    let pred' z = if z == lo then hi else pred z
        t:_ = dropWhile (`elem` [a, b, c]) . iterate pred' $ pred' x
    u <- readArray arr t
    writeArray arr x y
    writeArray arr t a
    writeArray arr c u
    pure y

newArray' :: [Int] -> ST s (STUArray s Int Int)
newArray' xs = do
    arr <- newArray_ (minimum xs, maximum xs)
    arr <$ zipWithM_ (writeArray arr) xs (drop 1 $ cycle xs)

part1 :: [Int] -> Int
part1 input = runST $ do
    let nums@(x:_) = input
    arr <- newArray' nums
    foldM_ (const . step arr) x $ replicate 100 ()
    let f i acc = readArray arr i >>= \case
            1 -> pure acc
            j -> f j $! 10 * acc + j
    f 1 0

part2 :: [Int] -> Int
part2 input = runST $ do
    let nums@(x:_) = input
    arr <- newArray' $ nums ++ [10..1000000]
    foldM_ (const . step arr) x $ replicate 10000000 ()
    y <- readArray arr 1
    z <- readArray arr y
    pure $ y * z