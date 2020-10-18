module Parser (detectSeparator) where

import Data.List (elemIndex)

contains :: Char -> [Char] -> Bool
contains char str = case elemIndex char str of
                      Just _ -> True
                      Nothing -> False

detectSeparator input 
    | contains ',' input = ","
    | contains '|' input = "|"
    | otherwise = " "

