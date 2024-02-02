{-# LANGUAGE OverloadedStrings #-}

module ParserSpec(spec) where

import Test.Hspec
import Parser (parse)
import TestHelper


-- tests one TestEntry
testEntry :: TestEntry -> SpecWith ()
testEntry entry = it ("Test nr. " ++ no entry ++ ": testing " ++ name entry) $ do
    let result = parse (input entry)
    case result of
        Left err -> expectationFailure err
        Right program -> show program `shouldBe` replaceAposQuotes (expected entry)

-- runs testEntry function on list of testEntrys
spec :: Spec
spec = describe "Parser Tests" $ do
    testData <- runIO (readTestData "test/examples/parserTests.json")
    mapM_ testEntry testData

-- #TODO: in helper file

replaceAposQuotes :: String -> String
replaceAposQuotes = map replaceChar
    where
        replaceChar '\'' = '"'
        replaceChar c    = c