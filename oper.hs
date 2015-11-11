infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2

infixl 6 |-|
a |-| b = if a>b then a-b else b-a

doubleFact :: Integer -> Integer
doubleFact (-1) = 1
doubleFact 0 = 1
doubleFact n = n * doubleFact (n-2)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
fibonacci n | n == 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci n | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' :: Integer -> Integer
fibonacci' n = helper' 0 1 n

helper' a b n | n == 0 = a
helper' a b n | n > 0 = helper' b (a+b) (n-1)
helper' a b n | n < 0 = helper' b (a-b) (n+1)

seqA :: Integer -> Integer
seqA n = let
         helper a _ _ 0 = a
         helper a b c x = helper b c (b+c-2*a) (x-1)
       in helper 1 2 3 n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
	| x == 0 = (0,1)
	| x > 0 = helper 0 0 x
	| x < 0 = helper 0 0 (-x)
		where
			helper sum cnt 0 = (sum,cnt)
			helper sum cnt x = helper (sum + x `mod` 10) (cnt + 1) (x `div` 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = onestep ((fa+fb)/2) (a+h) 1
	where
		n = 100000
		h = (b - a) / n
		fa = f a
		fb = f b
		onestep acc x i
			| i == n   = h*acc
			| otherwise  = onestep (acc+(f x)) (x+h) (i+1)