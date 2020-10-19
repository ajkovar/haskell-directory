{-# LANGUAGE OverloadedStrings #-}
module Parser (detectSeparator, parseLine, Person(Person, gender, lastName, dob), parseLines, parseDate) where

import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Text (strip, pack, unpack)
import Data.Time (parseTimeM, Day, defaultTimeLocale)
import Data.Maybe (catMaybes)

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , gender :: String  
                     , favoriteColor :: String  
                     , dob :: Day
                     } deriving (Eq) 

instance Show Person where
  show p = firstName p ++ " " ++ lastName p ++ " " ++ gender p ++ " " ++ favoriteColor p ++ " " ++ show (dob p)

contains :: Char -> [Char] -> Bool
contains char str = case elemIndex char str of
                      Just _ -> True
                      Nothing -> False

detectSeparator :: [Char] -> [Char]
detectSeparator input 
    | contains ',' input = ","
    | contains '|' input = "|"
    | otherwise = " "

parseDate  :: String -> Maybe Day
parseDate d = parseTimeM True defaultTimeLocale "%m/%d/%Y" d

parseArray :: [String] -> Maybe Person
parseArray [first, last, gender, color, dob] = 
  case parseDate dob of
    Just date -> Just $ Person first last gender color date
    Nothing -> Nothing

parseLine :: String -> Maybe Person
parseLine line = parseArray $ fmap strip' $ splitOn separator line
  where separator = detectSeparator line
        strip' = unpack . strip . pack

personInvalid :: Maybe Person -> Bool
personInvalid person = case person of
                         Just _ -> True
                         Nothing -> False 

parseLines :: String -> [Person]
parseLines file = catMaybes $ fmap parseLine $ filterBlank $ splitOn "\n" file
  where filterBlank = filter (\s -> (length s) > 0)
