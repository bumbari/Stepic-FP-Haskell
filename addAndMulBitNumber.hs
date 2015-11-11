import Data.List

data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

zero = Z Plus []
five = Z Plus [One, Zero, One]
minusEleven = Z Minus [One, One, Zero, One]

bitToNum :: Bit -> Int
bitToNum One  = 1
bitToNum _    = 0

signToNum :: Sign -> Int
signToNum Minus  = (-1)
signToNum _      = 1

bitsToNum :: [Bit] -> Int
bitsToNum bs = sum (zipWith (*) (map bitToNum bs) (unfoldr (\x -> Just(2^x,x+1)) 0))

zToNum :: Z -> Int
zToNum (Z s bs) = (signToNum s) * (bitsToNum bs)

numToBit :: Int -> Bit
numToBit 1 = One
numToBit _ = Zero

numToSign :: Int -> Sign
numToSign x | x < 0 = Minus
            | otherwise = Plus

numToZ :: Int -> Z
numToZ n = Z (numToSign n) (map numToBit (unfoldr (\x -> let (d,m) = divMod x 2 in if x == 0 then Nothing else Just(m,d)) (if n > 0 then n else (-n))))

add :: Z -> Z -> Z
add x y = numToZ $ (zToNum x) + (zToNum y)

mul :: Z -> Z -> Z
mul x y = numToZ $ (zToNum x) * (zToNum y)