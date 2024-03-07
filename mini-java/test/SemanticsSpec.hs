{-# LANGUAGE OverloadedStrings #-}

module SemanticsSpec(spec) where

import Test.Hspec
import TestHelper
import Semantics

-- tests one TestEntry
testEntry :: TestEntry -> SpecWith ()
testEntry entry = it ("Test nr. " ++ no entry ++ ": testing " ++ name entry) $ do
    let result = checkSemantics $ read $ replaceAposQuotes $ input entry
    case result of
        (t,[])  -> "true" `shouldBe` expected entry
        (t,es) -> "false" `shouldBe` expected entry

-- runs testEntry function on list of testEntrys
spec :: Spec
spec = describe "Semanticcheck Tests" $ do
    testData <- runIO (readTestData "test/examples/semanticsTests.json")
    mapM_ testEntry testData

replaceAposQuotes :: String -> String
replaceAposQuotes = map replaceChar
    where
        replaceChar '\'' = '"'
        replaceChar c    = c