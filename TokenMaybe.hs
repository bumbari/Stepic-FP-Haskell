import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken xs | all isDigit xs = Just $ Number (read xs :: Int)
asToken _ = Nothing

tokenize :: String -> Maybe [Token]
tokenize "" = Just []
tokenize input = helper l
    where
        l = words input
        helper []     = Just []
        helper (x:xs) = 
        	do
        		t <- asToken x
        		ts <- helper xs
        		return (t : ts)
