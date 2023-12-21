{-# LANGUAGE OverloadedStrings #-}

module SemanticsSpec where

import Test.Hspec
import TestHelper
import Semantics

-- #TODO: nicht nur false oder true zurück, sondern beschreibung was nicht geklappt hat
-- tests one TestEntry
testEntry :: TestEntry -> SpecWith ()
testEntry entry = it ("Test nr. " ++ no entry ++ ": testting " ++ name entry) $ do
    let result = checkSemantics $ read $ input entry
    case result of
        Left _  -> "false" `shouldBe` expected entry
        Right _ -> "true" `shouldBe` expected entry

-- runs testEntry function on list of testEntrys
spec :: Spec
spec = describe "Semanticcheck Tests" $ do
    testData <- runIO (readTestData "test/examples/semanticsTests.json")
    mapM_ testEntry testData