module Main (main) where

import Parser (parse)
import Syntax
import Semantics(checkSemantics)
import ClassFormat
import ConstPoolGen
import Data.Typeable
import ClassFileGen(generateClassFile)
import PrettyPrint
import BinaryClass


import System.Directory




main :: IO ()
main = do

    fileContent <- readFile "code/examples/helloWorld.minijava" -- read file
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
                putStrLn $ show t'
                putStrLn $ prettyPrintProgram t'
                let sampleCP = startBuildProcess t'
                putStrLn (show sampleCP)
                --let sampleCP = [Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},Class_Info {tag_cp = TagClass, index_cp = 1, desc = "java/lang/Object"},Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 3, index_descr_cp = 4, desc = "<init>:()V"},MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 5, desc = "java/lang/Object.<init>:()V"},Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 1, cad_cp = "A", desc = ""},Class_Info {tag_cp = TagClass, index_cp = 8, desc = "A"},MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 5, desc = ""},FieldRef_Info {tag_cp = TagFieldRef, index_name_cp = 2, index_nameandtype_cp = 11, desc = ""},NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 12, index_descr_cp = 13, desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "attr", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 1, cad_cp = "I", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 1, cad_cp = "j", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "meth", desc = ""},Utf8_Info {tag_cp = TagUtf8, tam_cp = 24, cad_cp = "(Ljava/lang/Boolean;)LA;", desc = ""}]
                putStrLn ""
                let constPoolShow = showCP_Infos sampleCP 1


                putStrLn constPoolShow
                putStrLn "Generating Class File"
                let sampleCF = generateClassFile t' sampleCP -- Todo
                let result = prettyPrintClassFile sampleCF -- Todo uncomment
                let classFileName = getClassNameFromProgram t'
                putStrLn result


                encodeClassFile (classFileName ++ ".class") sampleCF

                putStrLn ("The following ClassFile was generated: " ++ classFileName ++ ".class")
                --putStrLn ("sampleCF: " ++  show sampleCF)
                --print()
                --code <- decodeClassFile "/Users/anabelstammer/Documents/GitHub/Compilerbau/mini-java/EmptyClass.class"
                --putStrLn (show code)

                return ()
             (t',es) -> putStrLn $ prettyPrintProgram t'


-- For the class file
getClassNameFromProgram :: Program -> String
getClassNameFromProgram (Program classes _) =
    case classes of
        singleClass -> extractClassName singleClass
        _             -> "Class"

extractClassName :: Class -> String
extractClassName (Class className _ _) = newTypeToString className




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