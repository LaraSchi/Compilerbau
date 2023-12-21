{-# LANGUAGE DeriveGeneric #-}

module TestHelper where


import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B


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