module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List (findIndex)
import Data.Char (ord)

import Control.Monad.State
import Debug.Trace

-- TODO init wenn empty init method im AST -> erst nach Änderung in Syntax möglich mit eigenem return Typ für Init
-- TODO how to get max stack size in classfile?
-- TODO how does monad behave when method call happens
-- TODO branchoffset 
-- TODO CharLitExpr in Char
-- TODO test and check state monad usage:
-- TODO: unnötige Instr aus Datenstruktur schmeißen


----------------------------------------------------------------------
-- State Monad
-- Define the state type
data GlobalVars = GlobalVars
    { maxStackSize :: Int -- max stack size
    , currentStackSize :: Int -- current stack size
    , currentByteCodeSize :: Int -- current byte code array size
    , localVars :: [String] -- list of defined local variables to choose index for iload, istore, ...
    , typesOfLocalVars :: [Type]
    , returnType :: Type  -- method return type
    } deriving (Show)

-- Define a type synonym for the state monad
type GlobalVarsMonad = State GlobalVars

-- Getter
-- Function to get the current integer from the state
getMaxSize :: GlobalVarsMonad Int
getMaxSize = gets maxStackSize

getCurrentStackSize :: GlobalVarsMonad Int
getCurrentStackSize = gets currentStackSize

getCurrentByteCodeSize :: GlobalVarsMonad Int
getCurrentByteCodeSize = gets currentByteCodeSize

getReturnType :: GlobalVarsMonad Type
getReturnType = gets returnType

-- Function to get the current list from the state
getLocalVars :: GlobalVarsMonad [String]
getLocalVars = gets localVars

getLocalVarTypes :: GlobalVarsMonad [Type]
getLocalVarTypes = gets typesOfLocalVars


-- Function to add an integer to the max size
addToMaxStackSize :: Int -> GlobalVarsMonad ()
addToMaxStackSize x = modify (\s -> s { maxStackSize = maxStackSize s + x })

addToCurrentStackSize :: Int -> GlobalVarsMonad ()
addToCurrentStackSize x = modify (\s -> s { currentStackSize = currentStackSize s + x })

addToCurrentByteCodeSize :: Int -> GlobalVarsMonad ()
addToCurrentByteCodeSize x = modify (\s -> s { currentByteCodeSize = currentByteCodeSize s + x })

setReturnType :: Type -> GlobalVarsMonad ()
setReturnType x = modify (\s -> s { returnType = x })                                         -- ist das richtig so???

-- Function to add an element to the stack
addToLocalVars :: String -> GlobalVarsMonad ()
addToLocalVars x = modify (\s -> s { localVars = localVars s ++ [x] })

addToLocalVarTypes :: Type -> GlobalVarsMonad ()
addToLocalVarTypes x = modify (\s -> s { typesOfLocalVars = typesOfLocalVars s ++ [x] })



----------------------------------------------------------------------
-- Functions to generate Byte Code
-- Function to generate assembly code for init method
generateInitByteCode :: [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateInitByteCode cp = return ([ALoad_0] ++
                        [(InvokeSpecial 0x00 (getIndexByDesc ("java/lang/Object" ++ "." ++ "<init>" ++ ":" ++ "()V") cp))] -- reference to init method TODO: what if there are multiple inits
                        ++ [Return])

startBuildGenCodeProcess :: MethodDecl -> [CP_Info] -> [ByteCodeInstrs]
startBuildGenCodeProcess m cp =
    let (result, finalState) = runState (generateCodeForMethod m cp) initialState
    in result
    where
    initialState = GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, localVars = [], typesOfLocalVars = [], returnType = VoidT }
                        

{-
startBuildGenCodeProcess :: MethodDecl -> [CP_Info] -> [ByteCodeInstrs]
startBuildGenCodeProcess m cp = do

    --runState (generateCodeForMethod m cp) (GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, localVars = [], returnType = VoidT })
    let result = evalState (return []) (GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, localVars = [], returnType = VoidT })
    let result = evalState (generateCodeForMethod m cp) (GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, localVars = [], returnType = VoidT })

    result
-}

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethod (MethodDecl visibility retType name params stmt) cp_infos = 
    if retType == VoidT
        then do -- TODO params auf Stack check if empty init -> generateInitByteCode
            setReturnType retType                                                           
            stmtInstructions <- generateCodeForStmt stmt cp_infos
            return (stmtInstructions ++ [Return])
        else do
            setReturnType retType                                                           
            generateCodeForStmt stmt cp_infos
    

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmtList -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] cp_infos = return []
generateCodeForBlockStmt (stmt:stmts) cp_infos = do
    codeForStmt <- generateCodeForStmt stmt cp_infos
    codeForBlock <- generateCodeForBlockStmt stmts cp_infos
    return (codeForStmt ++ codeForBlock)

-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForStmt (TypedStmt stmt _) cp_infos = generateCodeForStmt stmt cp_infos
generateCodeForStmt (Block stmts) cp_infos = generateCodeForBlockStmt stmts cp_infos
-- Return
generateCodeForStmt (ReturnStmt expr) cp_infos = do 
    codeForExpr <- (generateCodeForExpression expr)
    retType <- getReturnType
    if retType == IntT || retType == BoolT || retType == CharT  
        then return (codeForExpr ++ [IReturn])
        else if retType == VoidT
            then return (codeForExpr ++ [Return])
            else return (codeForExpr ++ [AReturn])
-- While
generateCodeForStmt (WhileStmt expr (Block blockStmt)) cp_infos = do
    code <- generateCodeForBlockStmt blockStmt cp_infos
    code_expr <- generateCodeForExpression expr  -- current line number from monad to add branchoffset
    return (code ++ code_expr)
-- LocalVar Decl 
generateCodeForStmt (LocalVarDeclStmt var_type name maybeExpr) cp_infos = 
    case maybeExpr of
        Just expr -> do
            addToLocalVars name
            addToLocalVarTypes var_type
            varTypeList <- getLocalVarTypes
            localVarList <- getLocalVars
            codeForExpr <- generateCodeForExpression expr
            if var_type == BoolT || var_type == CharT || var_type == IntT
                then return (codeForExpr ++ [(getIStoreByIndex (getVarIndex name localVarList))])                                         -- ist das richtig so
                else return (codeForExpr ++ [(getAStoreByIndex (getVarIndex name localVarList))])  
        Nothing -> do
            addToLocalVars name
            addToLocalVarTypes var_type
            return []
-- If Else  !! nur ifcmp* werden verwendet; javac hat Sonderinstruktionen wenn Vergleich mit 0 durchgeführt wird, aber unnötig
generateCodeForStmt (IfElseStmt expr (Block blockStmt) maybeBlockStmt) cp_infos = do
    code <- generateCodeForBlockStmt blockStmt cp_infos
    code2 <- case maybeBlockStmt of
        Just (Block block) -> do
            codeForBlock <- generateCodeForBlockStmt block cp_infos
            return ([(Goto 0x0 0x0)] ++ codeForBlock)
        Nothing -> return []
    code_expr <- (generateCodeForIfElseStmtExpression expr 0x0 0x0)  -- current line number from monad to add branchoffset
    return(code_expr ++ code ++ code2)  -- goto muss irgendwo noch rein
-- Stmt Expr Stmt
generateCodeForStmt (StmtExprStmt stmtExpr) cp_infos = generateCodeForStmtExpr stmtExpr  -- TODO


{- Function to build byte code for if else statement
    currently only works with if_icmp* (so int, char and boolean) 
    -> need type of expr1 or expr2 to choose between if_icmp* and if_acmp*
-}
generateCodeForIfElseStmtExpression :: Expression -> Int -> Int -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForIfElseStmtExpression (BinOpExpr expr1 binop expr2) len1 len2 = do
    if_code <- case binop of
            Equal -> return [(If_ICmpNeq 0x0 0x0)]
            NotEqual -> return [(If_ICmpEq) 0x0 0x0]
            Less -> return [(If_ICmpGeq) 0x0 0x0]
            Greater -> return [(If_ICmpLeq) 0x0 0x0]
            LessEq -> return [(If_ICmpGt) 0x0 0x0]
            GreaterEq -> return [(If_ICmpLt) 0x0 0x0]
    codeForExpr1 <- generateCodeForExpression expr1
    codeForExpr2 <- generateCodeForExpression expr2
    return (codeForExpr1 ++ codeForExpr2 ++ if_code)

-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
-- Assign Stmt
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) = do
    codeExpr1 <- generateCodeForAssign expr1
    codeExpr2 <- generateCodeForExpression expr2
    case codeExpr1 of
        [(PutField _ _)] -> return ([ALoad_0] ++ codeExpr2 ++ codeExpr1)        -- wenn assignemnt auf field: aload dann was und dann putfield
        _ -> return (codeExpr2 ++ codeExpr1)                                                                   
-- New
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr  -- TODO ??
-- Method call
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr  -- TODO ??

generateCodeForAssign :: Expression -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForAssign (TypedExpr expr _) = generateCodeForAssign expr
generateCodeForAssign (FieldVarExpr name) = return [(PutField 0x0 0x0)]                 -- TODO verweis auf cp mit name
generateCodeForAssign (LocalVarExpr name) = do
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes
    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList
    if var_type == BoolT || var_type == IntT || var_type == CharT
        then return [(getIStoreByIndex (getVarIndex name varList))]
        else return [(getAStoreByIndex (getVarIndex name varList))]


generateCodeForMethodCallExpr :: MethodCallExpr -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) = do
    --generateCodeForExpression expr ++ generateCodeForExpressions exprList
    codeForExpr <- generateCodeForExpression expr
    codeForExprs <- generateCodeForExpressions exprList
    return (codeForExpr ++ codeForExprs)

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = return []                                                -- welcher Fall ist das?
generateCodeForExpression (SuperExpr) = return []                                               -- welcher Fall ist das?
generateCodeForExpression (FieldVarExpr name) = return [ALoad_0, (GetField 0x0 0x0)]            -- TODO referenz auf cp über name
generateCodeForExpression (LocalVarExpr name) = do                                              
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes
    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList
    if var_type == BoolT || var_type == IntT || var_type == CharT
        then return [(getILoadByIndex (getVarIndex name varList))]
        else return [(getALoadByIndex (getVarIndex name varList))]
generateCodeForExpression (InstVarExpr expr name) = generateCodeForExpression expr              -- TODO: eigentlich nur relevant, wenn man mehrere Klassen hat?
generateCodeForExpression (UnaryOpExpr un_op expr) = generateCodeForExpression expr
generateCodeForExpression (BinOpExpr expr1 _ expr2) = do
    codeExpr1 <- generateCodeForExpression expr1
    codeExpr2 <- generateCodeForExpression expr2
    return (codeExpr1 ++ codeExpr2)
generateCodeForExpression (IntLitExpr intVal) = return [(BIPush intVal)]
generateCodeForExpression (BoolLitExpr bool) = case bool of
    True -> return [(IConst_1)]
    False -> return [(IConst_0)]
generateCodeForExpression (CharLitExpr (character:str)) = return [(BIPush (ord character))]
generateCodeForExpression (StringLitExpr _) = return []                             -- noch nicht unterstützt
generateCodeForExpression (Null) = return [(AConst_Null)]
generateCodeForExpression (StmtExprExpr stmtExpr) = generateCodeForStmtExpr stmtExpr


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewExpr (NewExpr newType args) = return []
    --"new " ++ generateCodeForNewType newType ++ ", " ++ -- Todo get index in constantpool
    --"dup, " ++
    --generateCodeForExpressions args ++
    --"invokespecial " -- Todo index of a Method ref

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewType (NewType name) = return [] -- name  -- This is a simplistic approach

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpressions [] = return []
generateCodeForExpressions (expr:exprs) = do
    codeForExpr <- generateCodeForExpression expr
    codeForExprs <- generateCodeForExpressions exprs
    return (codeForExpr ++ codeForExprs)

----------------------------------------------------------------------
-- Helper functions to get list index of variable  (offset of 1 is accounted since load_0 and store_0 are not used)
getVarIndex :: String -> [String] -> Int
getVarIndex _ [] = -1                                                                   -- theoretisch müsste der error Fall noch abgefangen werden
getVarIndex var_name (var:vars)
    | var_name == var  = 1
    | otherwise = if indexRest == -1 then -1 else 1 + indexRest
    where indexRest = getVarIndex var_name vars

getTypeFromIndex :: Int -> [Type] -> Type
getTypeFromIndex index (t:ts) = 
    if (index - 1) == 0
        then t
        else getTypeFromIndex (index - 1) ts
    
----------------------------------------------------------------------
-- Helper functions to get store and load instr
getILoadByIndex :: Int -> ByteCodeInstrs
getILoadByIndex index = case index of
    0 -> ILoad_0
    1 -> ILoad_1
    2 -> ILoad_2
    3 -> ILoad_3
    _ -> (ILoad index)

getALoadByIndex :: Int -> ByteCodeInstrs
getALoadByIndex index = case index of
    0 -> ALoad_0
    1 -> ALoad_1
    2 -> ALoad_2
    3 -> ALoad_3
    _ -> (ALoad index)

getIStoreByIndex :: Int -> ByteCodeInstrs
getIStoreByIndex index = case index of
    0 -> IStore_0
    1 -> IStore_1
    2 -> IStore_2
    3 -> IStore_3
    _ -> (IStore index)

getAStoreByIndex :: Int -> ByteCodeInstrs
getAStoreByIndex index = case index of
    0 -> AStore_0
    1 -> AStore_1
    2 -> AStore_2
    3 -> AStore_3
    _ -> (AStore index)


----------------------------------------------------------------------
-- Helper function for constant pool

getIndexByDesc :: String -> [CP_Info] -> Int
getIndexByDesc descriptor cpList =
    case findIndex (\cpInfo -> desc cpInfo == descriptor) cpList of
        Just idx -> idx + 1 -- Constant pool begins at 1.
        Nothing  -> -1