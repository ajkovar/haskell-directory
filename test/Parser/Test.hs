module Parser.Test (suite) where

import qualified Test.HUnit as HUnit
import Parser (detectSeparator, parseLine, Person(Person))

suite :: HUnit.Test
suite = HUnit.TestList [testDetect, testParseLine]

testDetect :: HUnit.Test
testDetect = HUnit.TestLabel "detectSeparator" . HUnit.TestCase $ do
  HUnit.assertEqual "Detects ," "," $ detectSeparator "John, Smith"
  HUnit.assertEqual "Detects |" "|" $ detectSeparator "John | Smith"
  HUnit.assertEqual "Detects ' '" " " $ detectSeparator "John Smith"

testParseLine :: HUnit.Test
testParseLine = HUnit.TestLabel "parseLine" . HUnit.TestCase $ do
  HUnit.assertEqual "Should read comma separated" person (parseLine "John, Smith, male, red, 04/22/2021")
    where person = Person "John" "Smith" "male" "red" "04/22/2021"

-- contents <- readFile "test.txt"  