module Main where

import Parser (parseLines, Person(Person, gender, lastName, dob))
import System.IO (readFile)
import Data.Sort (sortBy)
import Data.List (intercalate)

compareGenderLastName p1 p2 = compare (gender p1, lastName p1) (gender p2, lastName p2)

printRecords r = intercalate "\n" $ fmap show r
  
main :: IO ()
main = do
  let files = ["data/commas.csv", "data/spaces.ssv", "data/pipes.psv"]
  records <- fmap (parseLines . concat) $ mapM readFile files

  putStrLn "\n\nSorted By gender / last name:"
  putStrLn $ printRecords $ sortBy compareGenderLastName records

  putStrLn "\n\nSorted By birth date ascending:"
  putStrLn $ printRecords $ sortBy (\p1 p2 -> compare (dob p1) (dob p2)) records

  putStrLn "\n\nSorted By last name descending:"
  putStrLn $ printRecords $ sortBy (\p1 p2 -> compare (lastName p2) (lastName p1)) records
  return ()
