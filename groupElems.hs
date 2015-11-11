groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = gacc xs [x] []
  where 
    gacc []     acc     all  = reverse $ acc:all
    gacc (x:xs) (z:acc) all | x == z    = gacc xs (z:z:acc) all
    gacc (x:xs) (z:acc) all | otherwise = gacc xs [x]       ((z:acc):all)