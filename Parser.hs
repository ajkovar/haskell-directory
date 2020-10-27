{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Parser (detectSeparator, parseLine, Person(Person, firstName, gender, lastName, dob, favoriteColor), fullName, parseLines, parseDate) where
import Data.Aeson
import GHC.Generics
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
                     } deriving (Generic, Eq) 

fullName :: Person -> String
fullName p = firstName p ++ " " ++ lastName p

instance Show Person where
  show p = fullName p ++ " " ++ gender p ++ " " ++ favoriteColor p ++ " " ++ show (dob p)

instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

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
parseArray _ = Nothing

parseLine :: String -> Maybe Person
parseLine line = parseArray $ fmap strip' $ splitOn separator line
  where separator = detectSeparator line
        strip' = unpack . strip . pack

parseLines :: String -> [Person]
parseLines file = catMaybes $ fmap parseLine $ splitOn "\n" file
