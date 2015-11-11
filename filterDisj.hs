filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _  _  [] = []
filterDisj f1 f2 (x:xs)
  | f1 x || f2 x = x : filterDisj f1 f2 xs
  | otherwise    = filterDisj f1 f2 xs