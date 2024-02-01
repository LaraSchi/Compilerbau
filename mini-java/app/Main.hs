module Main (main) where

import Parser (parse)
import Semantics(checkSemantics)
import ClassFormat (showCP_Infos, CP_Infos, CP_Info(..), Tag(..))
import ConstPoolGen (startBuildProcess)
import Data.Typeable
import ClassFileGen(generateClassFile)

main :: IO ()
main = do
    --fileContent <- readFile "code/example.minijava" -- read file
    fileContent <- readFile "code/examples/bct.minijava" -- read file
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
                let sampleCF = generateClassFile t' sampleCP
                let result = showCP_Infos sampleCP 1
                putStrLn result
                return ()
    -------------------------------------

{- 
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig? (z.B: ^))
                - eingebaute Funktionen, wie system.out.println? -}