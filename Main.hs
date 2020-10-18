module Main where

import qualified Parser (detectSeparator)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  return $ Parser.detectSeparator "hi"
  return ()
