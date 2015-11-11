data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where
    succ (Odd x) = Odd (x+2)
    pred (Odd x) = Odd (x-2)
    toEnum m | m `mod` 2 /= 0 = Odd (toInteger m)
             | otherwise      = error "Odd parameter cannot be even"
    fromEnum (Odd m) = fromIntegral m
    enumFrom x = x : enumFrom (succ x)
    enumFromThen x@(Odd n) y@(Odd m) = x : enumFromThen y (Odd (m+m-n))
    enumFromTo x@(Odd n) y@(Odd m) | n > m     = []
                                   | otherwise = x : enumFromTo (succ x) y
    enumFromThenTo x@(Odd n) y@(Odd m) z@(Odd k) | n > k && m-n > 0   = []
                                                 | n < k && m-n < 0   = []
                                                 | n <= k && m-n > 0  = x : enumFromThenTo y (Odd (m+(m-n))) z
                                                 | otherwise = x : enumFromThenTo y (Odd (m-(n-m))) z