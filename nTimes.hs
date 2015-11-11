nTimes:: a -> Int -> [a]
nTimes x n = helper n []
  where
    helper 0 acc = acc
    helper i acc = helper (i-1) (x:acc)