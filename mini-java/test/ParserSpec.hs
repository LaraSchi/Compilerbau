{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ParserSpec(spec) where

import Data.Aeson
import GHC.Generics
import Test.Hspec
import qualified Data.ByteString.Lazy as B
import Parser (parse)


data TestEntry = TestEntry
    { no :: String
    , name :: String
    , input :: String
    , expected :: String
    } deriving (Show, Generic)

instance FromJSON TestEntry
instance ToJSON TestEntry


-- Reads JSON into TestEntry
readTestData :: FilePath -> IO [TestEntry]
readTestData filePath = do
    jsonData <- B.readFile filePath
    let testData = eitherDecode jsonData :: Either String [TestEntry]
    case testData of
        Left err    -> error err
        Right data' -> return data'


-- tests one TestEntry
testEntry :: TestEntry -> SpecWith ()
testEntry entry = it ("Test nr. " ++ no entry ++ ": testting " ++ name entry) $ do
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