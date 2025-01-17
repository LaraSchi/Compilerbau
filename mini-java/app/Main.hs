module Main (main) where

import Parser (parse)
import Syntax
import Semantics(checkSemantics)
import ClassFormat
import ConstPoolGen
import ClassFileGen(generateClassFile)
import PrettyPrint
import BinaryClass
import System.Directory


-- For colour printing
import System.Console.ANSI


main :: IO ()
main = do
    -- CP Examples
    --fileContent <- readFile "code/ConstPoolExpls/explReferences.minijava"
    --fileContent <- readFile "code/ConstPoolExpls/constructor.minijava"
    --fileContent <- readFile "code/ConstPoolExpls/globalLocalSameName.minijava"
    --fileContent <- readFile "code/ConstPoolExpls/helloWorldClass.minijava"


    -- CF Examples
    --fileContent <- readFile "code/ClassFileGenExpls/methodsExpl.minijava"
    --fileContent <- readFile "code/ClassFileGenExpls/printExpl.minijava"
    --fileContent <- readFile "code/ClassFileGenExpls/branchExpl.minijava"
    --fileContent <- readFile "code/ClassFileGenExpls/negBranchExpl.minijava"


    --fileContent <- readFile "code/examples/explWhile.minijava" -- read file
    fileContent <- readFile "code/examples/helloWorld.minijava" -- read file

    putStrLn ""
    putStrLn "parsing file content"
    putStrLn ""
    putStrLn "File Content:"
    putStrLn fileContent
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics (addInit t) of
             (t',[]) -> do
                print t'
                putStrLn $ prettyPrintProgram t'
                let sampleCP = startBuildProcess t'
                putStrLn ""
                let constPoolShow = showCP_InfosWithColor sampleCP
                putStrLn constPoolShow
                putStrLn "Generating Class File"
                let sampleCF = generateClassFile t' sampleCP -- Todo
                let result = prettyPrintClassFile sampleCF -- Todo uncomment
                let classFileName = getClassNameFromProgram t'
                putStrLn result
                encodeClassFile ("GeneratedClassFiles/" ++ classFileName ++ ".class") sampleCF
                putStrLn ("The following ClassFile was generated: " ++ "GeneratedClassFiles/" ++ classFileName ++ ".class")
                --putStrLn ("sampleCF: " ++  show sampleCF)
                --print()
                --code <- decodeClassFile "/Users/anabelstammer/Documents/GitHub/Compilerbau/mini-java/EmptyClass.class"
                --putStrLn (show code)

                return ()
             (t',_) -> putStrLn $ prettyPrintProgram t'


-- For the class file
getClassNameFromProgram :: Program -> String
getClassNameFromProgram (Program classes _) =
    case classes of
        singleClass -> extractClassName singleClass
        _             -> "Class"

extractClassName :: Class -> String
extractClassName (Class className _ _) = newTypeToString className


-- If there is no defined Init function, an empty one is added to the code
addInit :: Program -> Program
addInit p@(Program (Class n@(NewType name) fs md) t) = if initMissing name md
    then Program (Class n fs (newInit:md)) t
    else p
        where newInit = MethodDecl Public VoidT name [] (Block [])

-- checks whether there is already a defined init function
initMissing :: String -> [MethodDecl] -> Bool
initMissing name = not . any (\(MethodDecl _ _ func _ _) -> name == func)

-- only for Parser & Semantik check
{- parseAndCheck :: String -> String -> IO ()
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
                mapM_ putStrLn $ reverse es
                putStrLn  "\x1b[0m"
                putStrLn $ prettyPrintProgram t'

checkAllExamples :: IO()
checkAllExamples = do
    let folder = "code/semantikCheck/"
    files1 <- listDirectory folder
    mapM_ (parseAndCheck folder) files1 -}
