data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p@(Person {firstName = fn}) = if length fn <= 2 then p else p {firstName = head fn : "."}
