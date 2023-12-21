module Main (main) where

import Parser (parse)

main :: IO ()
main = do
    fileContent <- readFile "code/examples/explIfElse.minijava" -- read file
    case parse fileContent of
        Left _  -> putStrLn "Term could not be parsed."
        Right t -> print t

{- 
TODO: 
    - Prüfen, ob Grammatik vollständig und ggf. erweitern.
        -> fehlt: 
                - forloops
                - Binary und Unary nich vollständig?) -}