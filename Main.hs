module Main where

import Parser (parseLines)
import System.IO (readFile)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  contents <- fmap parseLines $ readFile "commas.csv"  
  putStrLn $ show contents
  return ()
