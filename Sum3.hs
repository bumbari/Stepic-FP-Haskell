sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 l1 l2 l3 = sum2 (sum2 l1 l2 []) l3 []
  where
    sum2 []     []     acc = reverse acc
    sum2 []     ys     acc = sum2 [0] ys  acc
    sum2 xs     []     acc = sum2 xs  [0] acc
    sum2 (x:xs) (y:ys) acc = sum2 xs  ys  $ (x+y):acc