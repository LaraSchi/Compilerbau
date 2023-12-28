module Main (main) where

import Parser (parse)
import Semantics(checkSemantics)
import ClassFormat (showCP_Infos, CP_Infos, CP_Info(..), Tag(..))

main :: IO ()
main = do
    -- fileContent <- readFile "code/examples/bct.minijava" --  -- Anabels current fav minijava file :)
    fileContent <- readFile "code/advancedExamples/addN.minijava"   -- Laras current fav minijava file :)
    putStrLn ""
    putStrLn "parsing file content"
    putStrLn ""
    putStrLn "File Content:"
    putStrLn fileContent
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics t of
            Left _   -> print "false"
            Right t' -> print t'

    -- Example of a constant pool ------
    let sampleCP = [ Class_Info TagClass 1 "ExampleClass"
                    , FieldRef_Info TagFieldRef 2 3 "ExampleField"
                    , MethodRef_Info TagMethodRef 4 5 "ExampleMethod"
                    ]

    let result = showCP_Infos sampleCP 1
    putStrLn ""
    putStrLn "Example of constant pool"
    putStrLn result
    -------------------------------------

{- 
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig? (z.B: ^))
                - eingebaute Funktionen, wie system.out.println? -}