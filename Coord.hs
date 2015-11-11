data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter h (Coord x y) = Coord (h * (fromIntegral x) + h/2) (h * (fromIntegral y) + h/2)

getCell :: Double -> Coord Double -> Coord Int
getCell h (Coord x y) = Coord (floor (x/h)) (floor (y/h))