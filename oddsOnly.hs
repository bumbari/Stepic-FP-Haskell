oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
oddsOnly (_:xs) | otherwise = oddsOnly xs