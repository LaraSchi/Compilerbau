module CodeGen where


import Syntax
import ClassFormat

import Data.Typeable
import Parser (parse)
import Data.List (intercalate, nub, notElem, elemIndex, findIndex)

import Control.Monad (when, unless)
import Control.Monad.State
import Debug.Trace (traceShow)

-- State Monad
data ConstantpoolState = ConstantpoolState {constPool :: CP_Infos}
    deriving Show

type ConstantpoolStateM = State ConstantpoolState


getConstantpool :: ConstantpoolState -> [CP_Info]
getConstantpool = constPool

startBuildProcess :: Program -> [CP_Info]
startBuildProcess p = getConstantpool $ snd $ runState (do (generateConstantPool p)) $ ConstantpoolState []

-- Function to generate constant pool entries from the AST
{-# WARNING generateConstantPool "" #-}
generateConstantPool :: Program -> ConstantpoolStateM ()
generateConstantPool (Program classes typed_bool) = do
    generateClassConstantPool classes --Todo what if multiple classes?
    when (not typed_bool) $ do
            return error "Warning: Semantic analysis was not performed before constructing a constant pool." []

   --let result = traceShow "Input AST:"
   --                  . traceShow (show classes)
    --                 $ generateClassConstantPool classes


    -- concatMap generateClassConstantPool classes

-- Function to generate constant pool entries for a class
-- Todo init
generateClassConstantPool :: Class -> ConstantpoolStateM ()
generateClassConstantPool (Class className fields methods) = do
    addElements (createClassEntry 1 className ++
                [MethodRef_Info TagMethodRef 4 6 ""] ++ createClassEntry 4 (NewType "java/lang/Object") ++
                [NameAndType_Info TagNameAndType 7 8 ""] ++
                [createUtf8Entry "<init>"] ++ [createUtf8Entry "()V"])

    mapM_ (\field -> generateFieldConstantPool field className) fields
    mapM_ (\method -> generateMethodConstantPool method className) methods

-- Function to generate constant pool entries for a field
generateFieldConstantPool :: FieldDecl -> NewType -> ConstantpoolStateM ()
generateFieldConstantPool (FieldDecl fieldType fieldName) className = do
    currIdx <- getCurrentIdx
    classNameIdx <- getIdx (createUtf8Entry (newTypeToString className))
    addElement (FieldRef_Info TagFieldRef classNameIdx (currIdx+1) "")
    createNameAndTypeEntry fieldName fieldType


-- Function to generate constant pool entries for a method
generateMethodConstantPool ::  MethodDecl -> NewType -> ConstantpoolStateM ()
generateMethodConstantPool (MethodDecl visability this_type methodName parameters blockstmt) className = do
    currIdx <- getCurrentIdx
    classNameIdx <- getIdx (createUtf8Entry (newTypeToString className))
    addElement (MethodRef_Info TagMethodRef classNameIdx (currIdx+1) "")
    createNameAndTypeStringEntry methodName ("(" ++ (intercalate (concatMap getInputType parameters)) ++ ")" ++ (typeToString this_type))

   -- createUtf8Entry methodName : -- store method name
   -- [createUtf8Entry ("(" ++ (intercalate ", " (concatMap createInputString parameters)) ++ ")" ++ (typeToString this_type) )]++ -- store Input and Output type
   -- [NameAndType_Info TagNameAndType currentIndex (currentIndex+1) ""]++  -- name and type with index to method name and index to method types



-- Helper functions
-- Helper functions to create specific constant pool entries
createNameAndTypeEntry :: String -> Type -> ConstantpoolStateM ()
createNameAndTypeEntry thisName thisType = do
    nameAndTypeIndex <- getCurrentIdx
    addElement (NameAndType_Info TagNameAndType 0 0 "") -- 0 is just a placeholder
    addElements (createUtf8Entry thisName : [createUtf8Entry (typeToString thisType)])
    nameIdx <- getIdx (createUtf8Entry thisName)
    typeIdx <- getIdx (createUtf8Entry (typeToString thisType))
    modifyEntryAtIndex nameAndTypeIndex (NameAndType_Info TagNameAndType nameIdx typeIdx "")

createNameAndTypeStringEntry :: String -> String -> ConstantpoolStateM ()
createNameAndTypeStringEntry thisName thisTypeString = do
    nameAndTypeIndex <- getCurrentIdx
    addElement (NameAndType_Info TagNameAndType 0 0 "") -- 0 is just a placeholder
    addElements (createUtf8Entry thisName : [createUtf8Entry thisTypeString])
    nameIdx <- getIdx (createUtf8Entry thisName)
    typeIdx <- getIdx (createUtf8Entry thisTypeString)
    modifyEntryAtIndex nameAndTypeIndex (NameAndType_Info TagNameAndType nameIdx typeIdx "")


createClassEntry :: Int -> NewType -> [CP_Info]
createClassEntry currentIndex (NewType className) = Class_Info TagClass (currentIndex+1) "" : [createUtf8Entry className]

-- Function for creating Utf8Info from Type
createUtf8Entry :: String -> CP_Info
createUtf8Entry t = Utf8_Info TagUtf8 (length t) (t) ""


-- Additional supporting functions.

-- Map Type to String
typeToString :: Type -> String
typeToString t = case t of
  IntT -> "I" -- Todo wrapper classes of primitive types Ljava/lang/Integer (Object Type)
  BoolT -> "Z"
  CharT -> "C"
  StringT -> "java/lang/String;"  -- Object class
  NewTypeT nt -> newTypeToString nt
  FuncT args ret -> "(" ++ concatMap typeToString args ++ ")" ++ typeToString ret
  VoidT -> "V"

newTypeToString :: NewType -> String
newTypeToString (NewType className) = className

getInputType :: Parameter -> [String]
getInputType (Parameter this_type string) = [typeToString this_type]

-- Helper Functions to assist in handling the State Monad
addElement :: CP_Info -> ConstantpoolStateM ()
-- Only adds, if the list does not contain the element yet.
addElement x = do
    cp <- gets constPool
    unless (x `elem` cp) $ do
            modify (\s -> s { constPool = cp ++ [x] })

-- Function to add multiple elements to the list
addElements :: [CP_Info] -> ConstantpoolStateM ()
addElements xs = do
    cp <- gets constPool
    let newElements = filter (\x -> x `notElem` cp) (nub xs) -- no replications of elements
    modify (\s -> s { constPool = cp ++ newElements })

-- Modify an entry
modifyEntryAtIndex :: Int -> CP_Info -> ConstantpoolStateM ()
modifyEntryAtIndex index newEntry = do
    currentState <- get
    let currentEntries = constPool currentState
        updatedEntries = modifyAtIndex currentEntries index newEntry
    put $ currentState { constPool = updatedEntries }

modifyAtIndex :: [a] -> Int -> a -> [a]
modifyAtIndex list index newEntry = take (index-1) list ++ [newEntry] ++ drop index list

-- Get the index of a specific entry
getIdx :: CP_Info -> ConstantpoolStateM Int
getIdx cpInfo = do
    state <- get
    let maybeIdx = findIndex (== cpInfo) (constPool state)
    case maybeIdx of
        Just idx -> return (idx + 1) -- Constantpool begins at 1.
        Nothing  -> error "Element not found in constPool" -- Todo how to handle errors?


-- Get the index for the upcoming entry
getCurrentIdx :: ConstantpoolStateM Int
getCurrentIdx = do
    cp <- gets constPool
    return ((length cp) + 1)
