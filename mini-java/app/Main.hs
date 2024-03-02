module Main (main) where

import Parser (parse)
import Semantics(checkSemantics)
import ClassFormat
import ConstPoolGen (startBuildProcess)
import Data.Typeable
import ClassFileGen(generateClassFile)


main :: IO ()
main = do
    fileContent <- readFile "code/examples/explWhile.minijava" -- read file
    --fileContent <- readFile "code/examples/bct.minijava" -- read file
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
                let sampleCP = startBuildProcess t' -- Todo uncomment
                let constPoolShow = showCP_Infos sampleCP 1
                putStrLn constPoolShow

                -- let sampleCF = generateClassFile t' sampleCP -- Todo what is StackMapTable?
                -- let result = prettyPrintClassFile sampleCF -- Todo uncomment
                -- putStrLn result
                return ()
    -------------------------------------
{- 
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig? (z.B: ^))
                - eingebaute Funktionen, wie system.out.println? -}