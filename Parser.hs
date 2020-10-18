module Parser (detectSeparator, parseLine, Person(Person)) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , gender :: String  
                     , favoriteColor :: String  
                     , dob :: String  
                     } deriving (Show, Eq) 

contains :: Char -> [Char] -> Bool
contains char str = case elemIndex char str of
                      Just _ -> True
                      Nothing -> False

detectSeparator :: [Char] -> [Char]
detectSeparator input 
    | contains ',' input = ","
    | contains '|' input = "|"
    | otherwise = " "

parseArray :: [[Char]] -> Person
parseArray [first, last, gender, color, dob] = Person first last gender color dob

parseLine :: [Char] -> Person
parseLine line = parseArray $ splitOn (detectSeparator line) line

