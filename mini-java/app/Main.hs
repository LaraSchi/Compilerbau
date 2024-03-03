module Main (main) where

import Parser (parse)
import Semantics(checkSemantics)
import ClassFormat
import ConstPoolGen (startBuildProcess)
import Data.Typeable
-- import ClassFileGen(generateClassFile)

import System.Directory


main :: IO ()
main = do
    --fileContent <- readFile "code/semantikCheckExamples/missingReturn.minijava" -- read file
    fileContent <- readFile "code/examples/test.minijava" -- read file

    putStrLn ""
    putStrLn "parsing file content"
    putStrLn ""
    putStrLn "File Content:"
    putStrLn fileContent
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics t of
            Left _   -> print "false"
            Right t' -> do
                print t'
                let sampleCP = startBuildProcess t'
                putStrLn ""
                let constPoolShow = showCP_Infos sampleCP 1
                putStrLn constPoolShow

                -- let sampleCF = generateClassFile t' sampleCP -- Todo
                -- let result = prettyPrintClassFile sampleCF -- Todo uncomment
                -- putStrLn result
                return ()
    -------------------------------------
-- Laras Beispiel durchlauf Funktion
checkAllExamples :: IO()
checkAllExamples = do
    let folder = "code/semantikCheckExamples/"
    files1 <- listDirectory folder
    mapM_ (parseAndCheck folder) files1

parseAndCheck :: String -> String -> IO ()
parseAndCheck folder s = do
    fileContent <- readFile $ folder ++ s -- read file
    putStrLn ""
    putStrLn $ "parsing checked file content: " ++ s
    putStrLn ""
    putStrLn "File Content:"
    putStrLn fileContent
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics t of
            Left _   -> print "false"
            Right t' -> print t'


{-
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig? (z.B: ^))
                - eingebaute Funktionen, wie system.out.println? -}