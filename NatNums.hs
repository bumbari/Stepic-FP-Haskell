data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add  Zero   m = m
add (Suc n) m = Suc (add n m)

mul :: Nat -> Nat -> Nat
mul  Zero   _ = Zero
mul (Suc n) m = add m (mul n m)

fac :: Nat -> Nat
fac  Zero   = Suc Zero
fac (Suc n) = mul (Suc n) (fac n)