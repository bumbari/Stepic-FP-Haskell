import Data.Monoid

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor True) (Xor x) = Xor (not x)
    mappend _ q = q