import Data.List
import Data.Char
import Data.Maybe

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

fields = ["firstName","lastName","age"]
--str = "firstName = John\nlastName = Connor\nage = 30"
--str1 = "firstName = John\nlastName = = Connor\nage = 30"
--str2 = "firstName = John\nlasName = Connor\nage = 30"
--str3 = "firstName = John\nage = 30"
--str4 = "firstName = John\nnage = 30"
--str5 = "firstName = John\nlastName = Connor\nfoo\nage = 30"
--str6 = "firstName = John  \nlastName =  Connor\nage  = 30"
--str7 = "firstName = John  \nlastName =  Connor\nage  = 3x0"

splitString :: String -> [[String]]
splitString = map words . lines

parseEquals :: [String] -> Bool
parseEquals (_:"=":_) = True
parseEquals _         = False

checkParsing :: [[String]] -> Bool
checkParsing = all parseEquals

filterOnlyEquals :: [[String]] -> [[String]]
filterOnlyEquals  = filter parseEquals

makeNameAndFalue :: [String] -> (String,String)
makeNameAndFalue (f:"=":v:_) = (f,v)

checkJust :: Maybe a -> Bool
checkJust (Just _) = True
checkJust _        = False

parsePerson :: String -> Either Error Person
parsePerson str = if parseError then Left ParsingError
                  else if incompleteError then Left IncompleteDataError
                       else if incorrectAgeError then Left $ IncorrectDataError $ last allFields
                       	    else Right Person { firstName = head allFields, lastName = allFields !! 1, age = ageNum }
  where
    splitted = splitString str
    parseError = not $ checkParsing splitted
    allEqualsPairs = map makeNameAndFalue . filterOnlyEquals $ splitted
    allMaybeFields = map (flip lookup allEqualsPairs) fields
    incompleteError = not $ all checkJust allMaybeFields
    allFields = map fromJust allMaybeFields
    incorrectAgeError = not $ all isDigit $ last allFields
    ageNum = (read $ last allFields) :: Int