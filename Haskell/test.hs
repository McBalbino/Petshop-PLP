data Person = Person { 
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving (Show, Read)

main = do
    let p = read "Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}" :: Person
    print(show p)