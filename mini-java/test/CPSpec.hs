{-# LANGUAGE OverloadedStrings #-}

module CPSpec(spec) where

import Test.Hspec
import TestHelper
import ConstPoolGen
import ClassFormat
import Parser (parse)
import Semantics(checkSemantics)


-- tests one TestEntry
testEntry :: TestEntry -> SpecWith ()
testEntry entry = it ("Test nr. " ++ no entry ++ ": testing " ++ name entry) $ do
    case parse (input entry) of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics t of
            Left _   -> putStrLn "Semantics check failed."  -- Use putStrLn consistently
            Right t' -> let cpList = startBuildProcess t'
                        in showCP_Infos cpList 1 `shouldBe` replaceAposQuotes (expected entry)

    --case result of
     --   Left err -> expectationFailure err
      --  Right cp -> showCP_Infos cp 1 `shouldBe` replaceAposQuotes (expected entry)

-- runs testEntry function on list of testEntrys
spec :: Spec
spec = describe "CP Tests" $ do
    testData <- runIO (readTestData "test/examples/cpGenTests.json")
    mapM_ testEntry testData


replaceAposQuotes :: String -> String
replaceAposQuotes = map replaceChar
    where
        replaceChar '\'' = '"'
        replaceChar c    = c