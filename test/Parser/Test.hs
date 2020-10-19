module Parser.Test (suite) where

import qualified Test.HUnit as HUnit
import Parser (detectSeparator, parseLine, Person(Person), parseLines, parseDate)
import Data.Time (Day)
import Data.Maybe (fromJust)

suite :: HUnit.Test
suite = HUnit.TestList [testDetect, testParseLine, testParseLines]

parseDate' :: String -> Day
parseDate' s = fromJust $ parseDate s

testDetect :: HUnit.Test
testDetect = HUnit.TestLabel "detectSeparator" . HUnit.TestCase $ do
  HUnit.assertEqual "Detects ," "," $ detectSeparator "John, Smith"
  HUnit.assertEqual "Detects |" "|" $ detectSeparator "John | Smith"
  HUnit.assertEqual "Detects ' '" " " $ detectSeparator "John Smith"

testParseLine :: HUnit.Test
testParseLine = HUnit.TestLabel "parseLine" . HUnit.TestCase $ do
  HUnit.assertEqual "Should read comma separated" person (parseLine "John, Smith, male, red, 04/22/2021")
    where person = Just $ Person "John" "Smith" "male" "red" $ parseDate' "04/22/2021"

testParseLines :: HUnit.Test
testParseLines = HUnit.TestLabel "parseLines" . HUnit.TestCase $ do
  HUnit.assertEqual "Should read comma separated" [person1, person2] (parseLines file)
  HUnit.assertEqual "Should handle blank lines" [person1, person2] (parseLines fileWithBlanks)
    where person1 = Person "John" "Smith" "male" "red" $ parseDate' "04/22/2021"
          person2 = Person "Jane" "Smith" "female" "red" $ parseDate' "04/22/2022"
          file = "John, Smith, male, red, 04/22/2021\nJane, Smith, female, red, 04/22/2022"
          fileWithBlanks = "John, Smith, male, red, 04/22/2021\nJane, Smith, female, red, 04/22/2022"