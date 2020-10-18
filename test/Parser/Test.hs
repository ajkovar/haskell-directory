module Parser.Test (suite) where

import qualified Test.HUnit as HUnit
import Parser (detectSeparator)

suite :: HUnit.Test
suite = HUnit.TestList [testDetect]

testDetect :: HUnit.Test
testDetect = HUnit.TestLabel "detectSeparator" . HUnit.TestCase $ do
  HUnit.assertEqual "Detects ," "," $ detectSeparator "John, Smith"
  HUnit.assertEqual "Detects |" "|" $ detectSeparator "John | Smith"
  HUnit.assertEqual "Detects ' '" " " $ detectSeparator "John Smith"

