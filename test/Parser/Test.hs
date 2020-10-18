module Parser.Test (suite) where

import qualified Test.HUnit as HUnit
import qualified Parser (detectSeparator)

suite :: HUnit.Test
suite = HUnit.TestList [testDetect]

testDetect :: HUnit.Test
testDetect = HUnit.TestLabel "detectSeparator" . HUnit.TestCase $ do
  HUnit.assertEqual "Detects ," "," $ Parser.detectSeparator "John, Smith"