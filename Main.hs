module Main where

import Parser (parseLines, Person(Person, gender, lastName))
import System.IO (readFile)
import Data.Sort (sortBy)
import Data.List (intercalate)

sortGT p1 p2
  | gender p1 < gender p2 = GT
  | gender p1 > gender p2 = LT
  | gender p1 == gender p2 = compare (lastName p1) (lastName p2)

main :: IO ()
main = do
  let files = ["data/commas.csv", "data/spaces.ssv", "data/pipes.psv"]
  records <- fmap (parseLines . concat) $ mapM readFile files
  putStrLn "Sorted By gender / last name:"
  putStrLn $ intercalate "\n" $ fmap show $ sortBy sortGT records
  return ()
