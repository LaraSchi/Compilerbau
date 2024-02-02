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

-- Todo Wie funktioniert das dann mit den FieldRefs?
-- Todo Build desktiptor strings?

-- State Monad
data ConstantpoolState = ConstantpoolState {constPool :: CP_Infos}
    deriving Show

type ConstantpoolStateM = State ConstantpoolState

getConstantpool :: ConstantpoolState -> [CP_Info]
getConstantpool = constPool

startBuildProcess :: Program -> [CP_Info]
startBuildProcess p = getConstantpool $ snd $ runState (do (generateConstantPool p)) $ ConstantpoolState []

-- Helper Functions to assist in handling the State Monad
addElement :: CP_Info -> ConstantpoolStateM ()
-- Only adds, if the list does not contain the element yet.
addElement x = do
    cp <- gets constPool
    unless (x `elem` cp) $ do
            modify (\s -> s { constPool = cp ++ [x] })


modifyAtIndex :: [a] -> Int -> a -> [a]
modifyAtIndex list index newEntry = take (index-1) list ++ [newEntry] ++ drop index list


-- Modify an entry at a given index
modifyEntryAtIndex :: Int -> CP_Info -> ConstantpoolStateM ()
modifyEntryAtIndex index newEntry = do
    currentState <- get
    let currentEntries = constPool currentState
        updatedEntries = modifyAtIndex currentEntries index newEntry
    put $ currentState { constPool = updatedEntries }

-- Get the index of a specific entry. If it does not exist yet, -1 is returned .
getIdx :: CP_Info -> ConstantpoolStateM Int
getIdx cpInfo = do
    state <- get
    let maybeIdx = findIndex (== cpInfo) (constPool state)
    case maybeIdx of
        Just idx -> return (idx + 1) -- Constantpool begins at 1.
        Nothing  -> return (-1)

-- Get the index for the upcoming entry
getCurrentIdx :: ConstantpoolStateM Int
getCurrentIdx = do
    cp <- gets constPool
    return (if null cp then 1 else length cp + 1)

-- Generation
generateConstantPool :: Program -> ConstantpoolStateM ()
generateConstantPool (Program classes typed_bool) = do
    generateClassConstantPool classes
    when (not typed_bool) $ do
            return error "Warning: Semantic analysis was not performed before constructing a constant pool." []

data FieldOrMethod = ThisFieldDekl Field | ThisMethodDekl MethodDecl
    deriving (Show, Eq, Read)

generateClassConstantPool :: Class -> ConstantpoolStateM ()
generateClassConstantPool (Class className fields methods) = do
    -- Store the class -- Todo only one?
    _ <- createClassEntry className
    -- Store init
    generateMethodRefConstantPool "<init>" "()V" (NewType "java/lang/Object")
    -- Store declarations (Methods and Fields)
    mapM_ (\field -> generateFieldDeklCP field) fields
    mapM_ (\method -> generateMethodDeklCP method) methods
    -- Iterate over methods and store references (Methods and Fields)

    let fieldsAsFieldOrMethod = map ThisFieldDekl fields
    mapM_ (\method -> findReferencesMethodDecl method fieldsAsFieldOrMethod className) methods
    let methodsAsFieldOrMethod = map ThisMethodDekl methods
    mapM_ (\method -> findReferencesMethodDecl method methodsAsFieldOrMethod className) methods
    return ()


-- Find Field references my iterating over Methods block
findReferencesMethodDecl :: MethodDecl -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesMethodDecl (MethodDecl _ _ _ _ stmtList) fieldOrMethodDecls className =
    traceShow fieldOrMethodDecls $ findReferencesStmtList stmtList fieldOrMethodDecls className

findReferencesStmtList :: [Stmt] -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesStmtList stmtList fieldOrMethodDecls className =
    mapM_ (\thisStmt -> findReferencesStmt thisStmt fieldOrMethodDecls className) stmtList

findReferencesStmt :: Stmt -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesStmt stmt fieldOrMethodDecls className = case stmt of
  TypedStmt stmt thisType -> findReferencesStmt stmt fieldOrMethodDecls className
  ReturnStmt expr -> findReferencesExpr expr fieldOrMethodDecls className
  WhileStmt expr blockStmt -> do
    findReferencesExpr expr fieldOrMethodDecls className
    findReferencesStmtList blockStmt fieldOrMethodDecls className
  LocalVarDeclStmt thisType name -> do
        checkAndGenRef name fieldOrMethodDecls className -- Possibly add FieldRef
        when ((typeToString thisType) == (newTypeToString className)) $ do
              -- Possibly add class init MethodRef
              _ <- generateMethodRefConstantPool "<init>" "()V" (NewType (typeToString thisType))
              return ()
  IfStmt expr blockStmt -> do
    findReferencesExpr expr fieldOrMethodDecls className
    findReferencesStmtList blockStmt fieldOrMethodDecls className
  IfElseStmt expr blockStmt1 blockStmt2 -> do
    findReferencesExpr expr fieldOrMethodDecls className
    findReferencesStmtList blockStmt1 fieldOrMethodDecls className
    maybe (return ()) (\stmt -> findReferencesStmtList stmt fieldOrMethodDecls className) blockStmt2
  StmtExprStmt stmtExpr -> findReferencesStmtExpr stmtExpr fieldOrMethodDecls className
  _ -> (return ())


-- Todo just InstVar my FieldRef?
-- Todo Integer_Info
-- Todo printstatement


findReferencesExpr :: Expression -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findReferencesExpr expr fieldOrMethodDecls className = case expr of
  IdentifierExpr name -> do
    checkAndGenRef name fieldOrMethodDecls className
  InstVar expr name -> do
    findReferencesExpr expr fieldOrMethodDecls className
    checkAndGenRef name fieldOrMethodDecls className
  UnaryOpExpr _ e -> findReferencesExpr e fieldOrMethodDecls className
  BinOpExpr e1 _ e2 -> do
    findReferencesExpr e1 fieldOrMethodDecls className
    findReferencesExpr e2 fieldOrMethodDecls className
  StmtExprExpr stmtExpr -> findReferencesStmtExpr stmtExpr fieldOrMethodDecls className
  _ -> (return ())

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
findReferencesNewExpr (NewExpr _ exprList) fieldOrMethodDecls className = mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldOrMethodDecls className) exprList

findRefMethodCallExpr :: MethodCallExpr -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
findRefMethodCallExpr (MethodCallExpr expr name exprList) fieldOrMethodDecls className = do
    mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldOrMethodDecls  className) exprList
    trace "findRefMethodCallExpr" $ checkAndGenRef name fieldOrMethodDecls className
    trace "findRefMethodCallExpr" $ return ()


unwrapToFieldList :: [FieldOrMethod] -> [Field]
unwrapToFieldList decls = [field | ThisFieldDekl field <- decls]

unwrapToMethodList :: [FieldOrMethod] -> [MethodDecl]
unwrapToMethodList decls = [method | ThisMethodDekl method <- decls]

checkAndGenRef :: String -> [FieldOrMethod] -> NewType -> ConstantpoolStateM ()
checkAndGenRef name decls className = traceShow decls $ do

  case decls of
    (ThisFieldDekl _ : _) -> do
      let fieldRefs = filter (\(FieldDecl _ fieldName) -> name == fieldName) (unwrapToFieldList decls)
      mapM_ (\(FieldDecl fieldType fieldName) -> generateFieldRefConstantPool fieldName (typeToString fieldType) className) fieldRefs
      trace "checkAndGenRef" $ return ()
    (ThisMethodDekl _ : _) -> do

      let methodRefs = filter (\(MethodDecl _ _ methodName _ _) -> name == methodName) (unwrapToMethodList decls)
      mapM_ (\(MethodDecl _ thisType methodName parameters _) -> do
          let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString thisType)
          generateMethodRefConstantPool methodName methodType className) methodRefs
    _ -> trace "_" $ return ()

-- Helper functions to create specific constant pool entries
-- The Entries are added to the state and the Info is returned for index retrieval.

generateFieldDeklCP :: Field -> ConstantpoolStateM CP_Info
generateFieldDeklCP field = case field of
    (FieldDecl fieldType fieldName) -> do
        fieldNameInfo <- createUtf8Entry fieldName
        _ <- createUtf8Entry (typeToString fieldType)
        return fieldNameInfo
    (FieldRef fieldType fieldName) -> createUtf8Entry "FieldRefPlaceholder"  -- Placeholder entry for FieldRef


generateMethodDeklCP :: MethodDecl -> ConstantpoolStateM CP_Info
generateMethodDeklCP (MethodDecl _ this_type methodName parameters _) = do
    methodNameInfo <- createUtf8Entry methodName
    _ <- createUtf8Entry ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString this_type)
    _ <- createUtf8Entry "Code"
    return methodNameInfo

generateFieldRefConstantPool :: String -> String -> NewType -> ConstantpoolStateM CP_Info
generateFieldRefConstantPool fieldName thisType className = do
     classInfo <- createClassEntry className
     classNameIdx <- getIdx classInfo
     nameTypeInfo <- createNameAndTypeEntry fieldName thisType
     nameTypeIdx <- getIdx nameTypeInfo
     let deskr = (newTypeToString className) ++ "." ++ fieldName ++ ":" ++ thisType
     addElement (FieldRef_Info TagFieldRef classNameIdx nameTypeIdx deskr)
     return (FieldRef_Info TagFieldRef classNameIdx nameTypeIdx deskr)


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


-- Function for creating Utf8Info
createUtf8Entry :: String -> ConstantpoolStateM CP_Info
createUtf8Entry name = do
    utf8Info <- lift $ return $ Utf8_Info TagUtf8 (length name) name ""
    cp <- addElement utf8Info
    return utf8Info


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

-- Get the type
getInputType :: Parameter -> [String]
getInputType (Parameter this_type string) = [typeToString this_type]