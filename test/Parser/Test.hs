module Parser.Test (suite) where

import qualified Test.HUnit as HUnit
import Parser (detectSeparator, parseLine, Person(Person), parseLines)

suite :: HUnit.Test
suite = HUnit.TestList [testDetect, testParseLine, testParseLines]

testDetect :: HUnit.Test
testDetect = HUnit.TestLabel "detectSeparator" . HUnit.TestCase $ do
  HUnit.assertEqual "Detects ," "," $ detectSeparator "John, Smith"
  HUnit.assertEqual "Detects |" "|" $ detectSeparator "John | Smith"
  HUnit.assertEqual "Detects ' '" " " $ detectSeparator "John Smith"

testParseLine :: HUnit.Test
testParseLine = HUnit.TestLabel "parseLine" . HUnit.TestCase $ do
  HUnit.assertEqual "Should read comma separated" person (parseLine "John, Smith, male, red, 04/22/2021")
    where person = Person "John" "Smith" "male" "red" "04/22/2021"

testParseLines :: HUnit.Test
testParseLines = HUnit.TestLabel "parseLines" . HUnit.TestCase $ do
  HUnit.assertEqual "Should read comma separated" [person1, person2] (parseLines file)
    where person1 = Person "John" "Smith" "male" "red" "04/22/2021"
          person2 = Person "Jane" "Smith" "female" "red" "04/22/2022"
          file = "John, Smith, male, red, 04/22/2021\nJane, Smith, female, red, 04/22/2022"