module ConstPoolGen where

import Syntax
import ClassFormat

import Data.Typeable
import Parser
import Data.List (intercalate, nub, notElem, elemIndex, findIndex)

import Control.Monad (when, unless)
import Control.Monad.State
import Debug.Trace (traceShow)

import Debug.Trace (trace)


-- State Monad --------------------------------------------------
data ConstantpoolState = ConstantpoolState {constPool :: CP_Infos}
    deriving Show

type ConstantpoolStateM = State ConstantpoolState

getConstantpool :: ConstantpoolState -> [CP_Info]
getConstantpool = constPool

-- Helper Functions to assist in handling the State Monad
addElement :: CP_Info -> ConstantpoolStateM ()
-- Only adds, if the list does not contain the element yet.
addElement x = do
    cp <- gets constPool
    unless (x `elem` cp) $ do
            modify (\s -> s { constPool = cp ++ [x] })

{-
modifyAtIndex :: [a] -> Int -> a -> [a]
modifyAtIndex list index newEntry = take (index-1) list ++ [newEntry] ++ drop index list

-- Modify an entry at a given index
modifyEntryAtIndex :: Int -> CP_Info -> ConstantpoolStateM ()
modifyEntryAtIndex index newEntry = do
    currentState <- get
    let currentEntries = constPool currentState
        updatedEntries = modifyAtIndex currentEntries index newEntry
    put $ currentState { constPool = updatedEntries } -}

-- Get the index of a specific entry. If it does not exist yet, -1 is returned .
getIdx :: CP_Info -> ConstantpoolStateM Int
getIdx cpInfo = do
    state <- get
    let maybeIdx = findIndex (== cpInfo) (constPool state)
    case maybeIdx of
        Just idx -> return (idx + 1) -- Constantpool begins at 1.
        Nothing  -> return (-1)


-- Generation --------------------------------------------------
startBuildProcess :: Program -> [CP_Info]
startBuildProcess p = getConstantpool $ snd $ runState (do (generateConstantPool p)) $ ConstantpoolState []

generateConstantPool :: Program -> ConstantpoolStateM ()
generateConstantPool (Program classes typed_bool) = do
    generateClassConstantPool classes
    when (not typed_bool) $ do
            return error "Warning: Semantic analysis was not performed before constructing a constant pool." []

data FieldOrMethod = ThisFieldDekl Field | ThisMethodDekl MethodDecl
    deriving (Show, Eq, Read)

generateClassConstantPool :: Class -> ConstantpoolStateM ()
generateClassConstantPool (Class className fields methods) = do
    -- Store init
    generateMethodRefConstantPool "<init>" "()V" (NewType "java/lang/Object")
    -- Store the class
    _ <- createClassEntry className
    -- Store declarations (Methods and Fields)
    mapM_ (\field -> generateFieldDeklCP field className) fields
    mapM_ (\method -> generateMethodDeklCP method) methods
    -- Iterate over methods and store references (Methods and Fields)
    let methodsAsFieldOrMethod = map ThisMethodDekl methods
    mapM_ (\method -> findReferencesMethodDecl method methodsAsFieldOrMethod className) methods
    return ()


-- Find References my iterating over Methods block
findReferencesMethodDecl :: MethodDecl -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesMethodDecl (MethodDecl _ _ _ _ stmt) fieldOrMethodDecls className =
    findReferencesStmt stmt fieldOrMethodDecls className

findReferencesStmt :: Stmt -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesStmt stmt fieldOrMethodDecls className = case stmt of
  TypedStmt stmt thisType -> findReferencesStmt stmt fieldOrMethodDecls className
  Block blockstmt -> mapM_ (\thisStmt -> findReferencesStmt thisStmt fieldOrMethodDecls className) blockstmt
  ReturnStmt expr -> findReferencesExpr expr fieldOrMethodDecls className
  WhileStmt expr stmt -> do
    findReferencesExpr expr fieldOrMethodDecls className
    findReferencesStmt stmt fieldOrMethodDecls className
  LocalVarDeclStmt thisType name maybeExpr -> do
        case maybeExpr of
             Just expr -> findReferencesExpr expr fieldOrMethodDecls className
             Nothing -> return ()
  IfElseStmt expr stmt1 stmt2 -> do
    findReferencesExpr expr fieldOrMethodDecls className
    findReferencesStmt stmt1 fieldOrMethodDecls className
    maybe (return ()) (\stmt -> findReferencesStmt stmt fieldOrMethodDecls className) stmt2
  StmtExprStmt stmtExpr -> findReferencesStmtExpr stmtExpr fieldOrMethodDecls className
  Print stringToPrint -> do
    createStringEntry stringToPrint
    generateFieldRefConstantPool "out" "Ljava/io/PrintStream;" (NewType"java/lang/System")
    generateMethodRefConstantPool "println" "(Ljava/lang/String;)V" (NewType "java/io/PrintStream")
    return ()


findReferencesExpr :: Expression -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesExpr expr fieldOrMethodDecls className = case expr of
  TypedExpr (InstVarExpr (TypedExpr _ (NewTypeT className)) name) thisType -> generateFieldRefConstantPool name (typeToString thisType) className
  TypedExpr (FieldVarExpr fieldName) fieldType -> generateFieldRefConstantPool fieldName (typeToString fieldType) className
  TypedExpr e _ -> do
        findReferencesExpr e fieldOrMethodDecls className
  ThisExpr -> (return ())
  SuperExpr -> (return ())
  LocalOrFieldVarExpr name -> trace ("LocalOrFieldVarExpr is falsely found: " ++ name) $ return ()
  LocalVarExpr name -> (return ())
  InstVarExpr e name -> do
    findReferencesExpr e fieldOrMethodDecls className
  UnaryOpExpr _ e -> findReferencesExpr e fieldOrMethodDecls className
  BinOpExpr e1 _ e2 -> do
    findReferencesExpr e1 fieldOrMethodDecls className
    findReferencesExpr e2 fieldOrMethodDecls className
  IntLitExpr number -> (return ())
  BoolLitExpr bool -> (return ())
  CharLitExpr string -> (return ())
  StringLitExpr string -> (return ())
  Null -> (return ())
  StmtExprExpr stmtExpr -> findReferencesStmtExpr stmtExpr fieldOrMethodDecls className

findReferencesStmtExpr :: StmtExpr -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesStmtExpr stmtExpr fieldOrMethodDecls className = case stmtExpr of
  TypedStmtExpr stmtExpr _ -> do
    findReferencesStmtExpr stmtExpr fieldOrMethodDecls className
  AssignmentStmt expr1 expr2 -> do
    findReferencesExpr expr1 fieldOrMethodDecls className
    findReferencesExpr expr2 fieldOrMethodDecls className
  NewExpression newExpr -> findReferencesNewExpr newExpr fieldOrMethodDecls className
  MethodCall methodCallExpr -> do
    findRefMethodCallExpr methodCallExpr fieldOrMethodDecls className

findReferencesNewExpr :: NewExpr -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesNewExpr (NewExpr thisNewType exprList) fieldOrMethodDecls className = do
        when(newTypeToString thisNewType == newTypeToString className) $ do
            let methodType = if null exprList
                then "()V"
                else "(" ++ intercalate "" (map (\(TypedExpr _ t) -> typeToString t) exprList) ++ ")V"
            _ <- generateMethodRefConstantPool "<init>" methodType className
            mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldOrMethodDecls className) exprList
        mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldOrMethodDecls className) exprList

findRefMethodCallExpr :: MethodCallExpr -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findRefMethodCallExpr (MethodCallExpr expr name exprList) fieldOrMethodDecls className = do
    mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldOrMethodDecls  className) exprList
    resolveAndGenerateMethodRefs name fieldOrMethodDecls className exprList
    return ()

unwrapToFieldList :: [FieldOrMethod] -> [Field]
unwrapToFieldList decls = [field | ThisFieldDekl field <- decls]

unwrapToMethodList :: [FieldOrMethod] -> [MethodDecl]
unwrapToMethodList decls = [method | ThisMethodDekl method <- decls]

-- Check if it is a Reference. Is it a method of this class?
resolveAndGenerateMethodRefs :: String -> [FieldOrMethod] -> NewType -> [Expression] -> ConstantpoolStateM ()
resolveAndGenerateMethodRefs name decls className exprList = case decls of
    (ThisMethodDekl _ : _) -> do
      let methodRefs = filter (\(MethodDecl _ _ methodName prameters _) -> name == methodName && (areInputTypesCorrect exprList prameters)) (unwrapToMethodList decls)
      mapM_ (\(MethodDecl _ thisType methodName parameters _) -> do
          let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString thisType)
          generateMethodRefConstantPool methodName methodType className) methodRefs
    _ -> return ()

-- Check if the expression list fits the input parameters. Incase multiple Methods have the same name.
areInputTypesCorrect :: [Expression] -> [Parameter] -> Bool
areInputTypesCorrect exprs params = length exprs == length params && all fits (zip exprs params)
  where
    fits (TypedExpr _ exprType, Parameter paramType _) = exprType == paramType

----------------------------------------------------------------------------
-- Helper functions to create specific constant pool entries
-- The Entries are added to the state and the Info is returned for index retrieval.
generateFieldDeklCP :: Field -> NewType -> ConstantpoolStateM ()
generateFieldDeklCP field className = case field of
    (FieldDecl fieldType fieldName Nothing) -> do
        fieldNameInfo <- createUtf8Entry fieldName
        _ <- createUtf8Entry (typeToString fieldType)
        return ()
    (FieldDecl fieldType fieldName expr) -> generateFieldRefConstantPool fieldName (typeToString fieldType) className

generateMethodDeklCP :: MethodDecl -> ConstantpoolStateM CP_Info
generateMethodDeklCP (MethodDecl _ this_type methodName parameters _) = do
    methodNameInfo <- createUtf8Entry methodName
    _ <- createUtf8Entry ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString this_type)
    _ <- createUtf8Entry "Code"
    return methodNameInfo

generateFieldRefConstantPool :: String -> String -> NewType -> ConstantpoolStateM ()
generateFieldRefConstantPool fieldName thisType className = do
     classInfo <- createClassEntry className
     classNameIdx <- getIdx classInfo
     nameTypeInfo <- createNameAndTypeEntry fieldName thisType
     nameTypeIdx <- getIdx nameTypeInfo
     let deskr = (newTypeToString className) ++ "." ++ fieldName ++ ":" ++ thisType
     addElement (FieldRef_Info TagFieldRef classNameIdx nameTypeIdx deskr)

-- Function to generate constant pool entries for a method references
generateMethodRefConstantPool :: String -> String -> NewType -> ConstantpoolStateM CP_Info
generateMethodRefConstantPool methodName thisType className = do
      classInfo <- createClassEntry className
      classNameIdx <- getIdx classInfo
      nameTypeInfo <- createNameAndTypeEntry methodName thisType
      nameTypeIdx <- getIdx nameTypeInfo
      let deskr = newTypeToString(className) ++ "." ++ methodName ++ ":" ++ thisType
      addElement (MethodRef_Info TagMethodRef classNameIdx nameTypeIdx deskr)
      createUtf8Entry "Code"
      return (MethodRef_Info TagMethodRef classNameIdx nameTypeIdx deskr)


createNameAndTypeEntry :: String -> String -> ConstantpoolStateM CP_Info
createNameAndTypeEntry thisName thisType = do
    -- Store name and type as utf8
    nameInfo <- createUtf8Entry thisName
    typeInfo <- createUtf8Entry thisType
    -- get their indexes.
    nameIdx <- getIdx nameInfo
    typeIdx <- getIdx typeInfo
    let deskr = thisName ++ ":" ++ thisType
    addElement (NameAndType_Info TagNameAndType nameIdx typeIdx deskr)
    return (NameAndType_Info TagNameAndType nameIdx typeIdx deskr)


createClassEntry :: NewType -> ConstantpoolStateM CP_Info
createClassEntry (NewType className) = do
     utf8Info <- (createUtf8Entry className)
     classNameIdx <- getIdx utf8Info
     let deskr = className
     addElement (Class_Info TagClass classNameIdx deskr)
     return (Class_Info TagClass classNameIdx deskr)

createStringEntry :: String -> ConstantpoolStateM CP_Info
createStringEntry stringToPrint = do
     utf8Info <- (createUtf8Entry stringToPrint)
     stringToPrintIdx <- getIdx utf8Info
     let deskr = stringToPrint
     addElement (String_Info TagString stringToPrintIdx deskr)
     return (String_Info TagString stringToPrintIdx deskr)


-- Function for creating Utf8Info
createUtf8Entry :: String -> ConstantpoolStateM CP_Info
createUtf8Entry name = do
    utf8Info <- lift $ return $ Utf8_Info TagUtf8 (length name) name ""
    cp <- addElement utf8Info
    return utf8Info


-- Map Type to String
typeToString :: Type -> String
typeToString t = case t of
  IntT -> "I"
  BoolT -> "Z"
  CharT -> "C"
  StringT -> "java/lang/String;"  -- Object class
  NewTypeT nt -> ("L" ++ newTypeToString  nt)
  FuncT args ret -> "(" ++ concatMap typeToString args ++ ")" ++ typeToString ret
  VoidT -> "V"

newTypeToString :: NewType -> String
newTypeToString (NewType className) = case className of
    "void" -> "V"
    _ -> className

-- Get the type
getInputType :: Parameter -> [String]
getInputType (Parameter this_type string) = [typeToString this_type]


