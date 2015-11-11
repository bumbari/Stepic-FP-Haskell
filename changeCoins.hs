import Data.List

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change n = [c:r | (c:cs) <- tails coins, c <= n, r <- change (n-c)]