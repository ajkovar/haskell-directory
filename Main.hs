module Main where

import Parser (parseLines)
import System.IO (readFile)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let files = ["data/commas.csv", "data/spaces.ssv", "data/pipes.psv"]
  records <- fmap (parseLines . concat) $ mapM readFile files
  putStrLn $ show records
  return ()
