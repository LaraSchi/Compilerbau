module Main (main) where

import Parser (parse)
import Semantics(checkSemantics)

main :: IO ()
main = do
    fileContent <- readFile "code/example.minijava" -- read file
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> case checkSemantics t of
            Left e   -> print "false"
            Right t' -> print "true"

{- 
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig?) -}