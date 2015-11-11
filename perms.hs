perms :: [a] -> [[a]]
perms []     = [[]]
perms [x]    = [[x]]
perms (x:xs) = ins x `concatMap` perms xs
  where
    ins :: a -> [a] -> [[a]]
    ins x y = map (\p -> (take p y) ++ [x] ++ (drop p y)) [0..(length y)]
