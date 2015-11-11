qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

recurSum :: [Int] -> Int
recurSum list = if null list then 0 else head list + recurSum (tail list)

recurMul :: [Int] -> Int
recurMul list = if null list then 1 else head list * recurMul (tail list)

recurLen :: [Int] -> Int
recurLen list = if null list then 0 else 1 + recurLen (tail list)


recurOp op nach list = if null list then nach else op (head list) (recurOp op nach (tail list))