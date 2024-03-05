module Main (main) where

import Parser (parse)
import Syntax
import Semantics(checkSemantics)
import ClassFormat
import ConstPoolGen (startBuildProcess)
import Data.Typeable
import ClassFileGen(generateClassFile)
import PrettyPrint


import System.Directory




main :: IO ()
main = do


    fileContent <- readFile "code/examples/explFieldRef.minijava" -- read file
    --fileContent <- readFile "code/examples/bct.minijava" -- read file

    putStrLn ""
    putStrLn "parsing file content"
    putStrLn ""
    putStrLn "File Content:"
    putStrLn fileContent
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics (addInit t) of
             (t',[]) -> do
                putStrLn $ prettyPrintProgram t'
                let sampleCP = startBuildProcess t'
                putStrLn ""
                let constPoolShow = showCP_Infos sampleCP 1
                putStrLn constPoolShow
                putStrLn "Generating Class File"
                let sampleCF = generateClassFile t' sampleCP -- Todo
                let result = prettyPrintClassFile sampleCF -- Todo uncomment
                putStrLn result
                return ()
             (t',es) -> putStrLn $ prettyPrintProgram t'

-- if there is no defined Init function, an empty one is added to the code
addInit :: Program -> Program
addInit p@(Program (Class n@(NewType name) fs md) t) = if initMissing name md
    then Program (Class n fs (init:md)) t
    else p
        where init = MethodDecl Public VoidT name [] (Block [])

-- checks whether there is already a defined init function
initMissing :: String -> [MethodDecl] -> Bool
initMissing name = not . any (\(MethodDecl _ _ func _ _) -> name == func)
{-
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig? (z.B: ^))
                - eingebaute Funktionen, wie system.out.println? -}


    -------------------------------------
-- Laras Beispiel durchlauf Funktion TODO: rausschmeißen


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
            (t',[]) -> putStrLn $ prettyPrintProgram t'
            (t',es) -> do
                putStrLn "\x1b[31m The following semantic error(s) were detected: "
                mapM_ putStrLn es
                putStrLn  "\x1b[0m"
                putStrLn $ prettyPrintProgram t'

checkAllExamples :: IO()
checkAllExamples = do
    let folder = "code/semantikCheck/"
    files1 <- listDirectory folder
    mapM_ (parseAndCheck folder) files1