module CodeGen where

import Syntax
import ClassFormat

import Data.Typeable
import Parser
import Data.List (intercalate, nub, notElem, elemIndex, findIndex)

import Control.Monad (when, unless)
import Control.Monad.State
import Debug.Trace (traceShow)


import Debug.Trace (trace)

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

-- Function to add multiple elements to the list
--addElements :: [CP_Info] -> ConstantpoolStateM ()
--addElements xs = do
--    cp <- gets constPool
--    let newElements = filter (\x -> x `notElem` cp) (nub xs) -- no replications of elements
--    modify (\s -> s { constPool = cp ++ newElements })

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

generateClassConstantPool :: Class -> ConstantpoolStateM ()
generateClassConstantPool (Class className fields methods) = do
    -- Store the class -- Todo only one?
    _ <- createClassEntry className
    -- Store init
    generateMethodRefConstantPool "<init>" "()V" (NewType "java/lang/Object")-- Todo what is not default
    -- Store declarations (Methods and Fields)
    mapM_ (\field -> generateFieldDeklCP field) fields
    mapM_ (\method -> generateMethodDeklCP method) methods
    -- Iterate over methods and store references (Methods and Fields)
    mapM_ (\method -> findReferencesMethodDecl method fields className) methods
    return ()

-- Find Field references my iterating over Methods block
findReferencesMethodDecl :: MethodDecl -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesMethodDecl (MethodDecl _ _ _ _ stmtList) fieldDecls className =
  findReferencesStmtList stmtList fieldDecls className

findReferencesStmtList :: [Stmt] -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesStmtList stmtList fieldDecls className =
    mapM_ (\thisStmt -> findReferencesStmt thisStmt fieldDecls className) stmtList

findReferencesStmt :: Stmt -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesStmt stmt fieldDecls className = case stmt of
  TypedStmt stmt thisType -> findReferencesStmt stmt fieldDecls className
  ReturnStmt expr -> findReferencesExpr expr fieldDecls className
  WhileStmt expr blockStmt -> do
    findReferencesExpr expr fieldDecls className
    findReferencesStmtList blockStmt fieldDecls className
  LocalVarDeclStmt thisType name -> do
        checkAndGenFieldRef name fieldDecls className -- Possibly add FieldRef
        when ((typeToString thisType) == (newTypeToString className)) $ do
              -- Possibly add class init MethodRef
              _ <- generateMethodRefConstantPool "<init>" "()V" (NewType (typeToString thisType))
              return ()
  IfStmt expr blockStmt -> do
    findReferencesExpr expr fieldDecls className
    findReferencesStmtList blockStmt fieldDecls className
  IfElseStmt expr blockStmt1 blockStmt2 -> do
    findReferencesExpr expr fieldDecls className
    findReferencesStmtList blockStmt1 fieldDecls className
    maybe (return ()) (\stmt -> findReferencesStmtList stmt fieldDecls className) blockStmt2
  StmtExprStmt stmtExpr -> findReferencesStmtExpr stmtExpr fieldDecls className
  _ -> (return ())

findReferencesExpr :: Expression -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesExpr expr fieldDecls className = case expr of
  IdentifierExpr name -> checkAndGenFieldRef name fieldDecls className
  InstVar expr name -> do
    trace ("Found instance variable: " ++ show name) findReferencesExpr expr fieldDecls className
    checkAndGenFieldRef name fieldDecls className
  UnaryOpExpr _ e -> findReferencesExpr e fieldDecls className
  BinOpExpr e1 _ e2 -> do
    findReferencesExpr e1 fieldDecls className
    findReferencesExpr e2 fieldDecls className
  StmtExprExpr stmtExpr -> findReferencesStmtExpr stmtExpr fieldDecls className
  _ -> (return ())

findReferencesStmtExpr :: StmtExpr -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesStmtExpr stmtExpr fieldDecls className = case stmtExpr of
  TypedStmtExpr stmtExpr _ -> findReferencesStmtExpr stmtExpr fieldDecls className
  AssignmentStmt expr1 expr2 -> do
    findReferencesExpr expr1 fieldDecls className
    findReferencesExpr expr2 fieldDecls className
  NewExpression newExpr -> findReferencesNewExpr newExpr fieldDecls className
  MethodCall methodCallExpr -> findRefMethodCallExpr methodCallExpr fieldDecls className

findReferencesNewExpr :: NewExpr -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findReferencesNewExpr (NewExpr _ exprList) fieldDecls className = mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldDecls className) exprList

findRefMethodCallExpr :: MethodCallExpr -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
findRefMethodCallExpr (MethodCallExpr expr name exprList)  fieldDecls className = do
    checkAndGenFieldRef name fieldDecls className
    mapM_ (\thisExpr -> findReferencesExpr thisExpr fieldDecls className) exprList

checkAndGenFieldRef :: String -> [FieldDecl] -> NewType -> ConstantpoolStateM ()
checkAndGenFieldRef name fieldDecls className = do
  let fieldRefs = filter (\(FieldDecl _ fieldName) -> name == fieldName) fieldDecls
  mapM_ (\(FieldDecl fieldType fieldName) -> generateFieldRefConstantPool fieldName (typeToString fieldType) className) fieldRefs


-- Helper functions to create specific constant pool entries
-- The Entries are added to the state and the Info is returned for index retrieval.

generateFieldDeklCP :: FieldDecl -> ConstantpoolStateM CP_Info
generateFieldDeklCP (FieldDecl fieldType fieldName) = do
    fieldNameInfo <- createUtf8Entry fieldName
    _ <- createUtf8Entry (typeToString fieldType)
    return fieldNameInfo

generateMethodDeklCP :: MethodDecl -> ConstantpoolStateM CP_Info
generateMethodDeklCP (MethodDecl _ this_type methodName parameters _) = do
    methodNameInfo <- createUtf8Entry methodName
    _ <- createUtf8Entry ("(" ++ intercalate " " (concatMap getInputType parameters) ++ ")" ++ typeToString this_type)
    _ <- createUtf8Entry "Code"
    return methodNameInfo


generateFieldRefConstantPool :: String -> String -> NewType -> ConstantpoolStateM CP_Info
generateFieldRefConstantPool fieldName thisType className = do
     classInfo <- createClassEntry className
     classNameIdx <- getIdx classInfo
     nameTypeInfo <- createNameAndTypeEntry fieldName thisType
     nameTypeIdx <- getIdx nameTypeInfo
     addElement (FieldRef_Info TagFieldRef classNameIdx nameTypeIdx "")
     return (FieldRef_Info TagFieldRef classNameIdx nameTypeIdx "")


-- Function to generate constant pool entries for a method references
generateMethodRefConstantPool :: String -> String -> NewType -> ConstantpoolStateM CP_Info
generateMethodRefConstantPool methodName thisType className = do
      classInfo <- createClassEntry className
      classNameIdx <- getIdx classInfo
      nameTypeInfo <- createNameAndTypeEntry methodName thisType
      nameTypeIdx <- getIdx nameTypeInfo
      addElement (MethodRef_Info TagMethodRef classNameIdx nameTypeIdx "")
      createUtf8Entry "Code"
      return (MethodRef_Info TagMethodRef classNameIdx nameTypeIdx "")

    -- Todo  nameTypeCpInfo <- createNameAndTypeStringEntry methodName ("(" ++ intercalate " " (concatMap getInputType parameters) ++ ")" ++ typeToString this_type)


createNameAndTypeEntry :: String -> String -> ConstantpoolStateM CP_Info
createNameAndTypeEntry thisName thisType = do
    -- Store name and type as utf8
    nameInfo <- createUtf8Entry thisName
    typeInfo <- createUtf8Entry thisType
    -- get their indexes.
    nameIdx <- getIdx nameInfo
    typeIdx <- getIdx typeInfo
    addElement (NameAndType_Info TagNameAndType nameIdx typeIdx "")
    return (NameAndType_Info TagNameAndType nameIdx typeIdx "")


createClassEntry :: NewType -> ConstantpoolStateM CP_Info
createClassEntry (NewType className) = do
     utf8Info <- (createUtf8Entry className)
     classNameIdx <- getIdx utf8Info
     addElement (Class_Info TagClass classNameIdx "")
     return (Class_Info TagClass classNameIdx "")


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