module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List (findIndex)
import Data.Char (ord)
import Data.Bits
import Control.Monad.State
import ConstPoolGen
import Debug.Trace

-- TODO how to get max stack size in classfile?
-- TODO branchoffset 
-- TODO unary: Not
-- TODO CharLitExpr in Char
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
    , className :: String
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

getClassName :: GlobalVarsMonad String
getClassName = gets className


-- Function to add an integer to the max size
addToMaxStackSize :: Int -> GlobalVarsMonad ()
addToMaxStackSize x = modify (\s -> s { maxStackSize = maxStackSize s + x })

addToCurrentStackSize :: Int -> GlobalVarsMonad ()
addToCurrentStackSize x = modify (\s -> s { currentStackSize = currentStackSize s + x })

addToCurrentByteCodeSize :: Int -> GlobalVarsMonad ()
addToCurrentByteCodeSize x = modify (\s -> s { currentByteCodeSize = currentByteCodeSize s + x })

setReturnType :: Type -> GlobalVarsMonad ()
setReturnType x = modify (\s -> s { returnType = x })                                         

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
                        [(InvokeSpecial 0x00 (getIndexByDesc ("java/lang/Object" ++ "." ++ "<init>" ++ ":" ++ "()V") cp))]
                        ++ [Return])

startBuildGenCodeProcess :: MethodDecl -> [CP_Info] -> String -> [ByteCodeInstrs]
startBuildGenCodeProcess m cp className =
    let (result, finalState) = runState (generateCodeForMethod m cp) initialState
    in result
    where
    initialState = GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, localVars = [], typesOfLocalVars = [], returnType = VoidT, className = className }
                        

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethod (MethodDecl visibility retType name params stmt) cp_infos = do
    className <- getClassName
    let initCode =
            if name == className
                then let deskr = (className ++ "." ++ "<init>" ++ ":()V")
                         idx = getIndexByDesc deskr cp_infos 
                     -- in [(ALoad_0), (InvokeSpecial ((idx `shiftR` 8) .&. 0xFF) (idx .&. 0xFF))]
                     in [(ALoad_0), (InvokeSpecial 0x0 idx)]                                                    -- TODO CP Ref
                else []
    addParamsToMonad params
    if retType == VoidT
        then do -- TODO params auf Stack check if empty init -> generateInitByteCode
            setReturnType retType                                                           
            stmtInstructions <- generateCodeForStmt stmt cp_infos
            let code = initCode ++ stmtInstructions ++ [Return]
            addToCurrentByteCodeSize 1
            return (code)                                                       -- TODO: wenn aber return dran steht, dann gibt es zweimal return!!! -> geht eh nicht durch den Parser
        else do
            setReturnType retType
            code <- generateCodeForStmt stmt cp_infos
            return (code)
    

addParamsToMonad :: [Parameter] -> GlobalVarsMonad [ByteCodeInstrs]
addParamsToMonad [] = return []
addParamsToMonad ((Parameter p_type p_name):params) = do
    addToLocalVars p_name
    addToLocalVarTypes p_type
    addParamsToMonad params

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmtList -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
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
        then do
            let code = codeForExpr ++ [IReturn]
            addToCurrentByteCodeSize 1
            return (code)
        else if retType == VoidT
            then do
                let code = codeForExpr ++ [Return]
                addToCurrentByteCodeSize 1
                return (code)
            else do
                let code = codeForExpr ++ [AReturn]
                addToCurrentByteCodeSize 1
                return (code)

-- While
generateCodeForStmt (WhileStmt expr (Block blockStmt)) cp_infos = do                                            -- TODO
    code <- generateCodeForBlockStmt blockStmt cp_infos
    code_expr <- generateCodeForExpression expr
    return (code ++ code_expr)
-- LocalVar Decl 
generateCodeForStmt (LocalVarDeclStmt var_type name maybeExpr) cp_infos = 
    case maybeExpr of
        Just expr -> do
            addToLocalVars name
            addToLocalVarTypes var_type
            localVarList <- getLocalVars
            codeForExpr <- generateCodeForExpression expr
            let codeWithoutPop =
                    if isExprWithPopInstr expr
                        then init codeForExpr -- delete last element (Pop instr.)
                        else codeForExpr
            if var_type == BoolT || var_type == CharT || var_type == IntT
                then do
                    let index = (getVarIndex name localVarList)
                    if index <= 3
                        then addToCurrentByteCodeSize 1
                        else addToCurrentByteCodeSize 2
                    return (codeWithoutPop ++ [(getIStoreByIndex index)])
                else do
                    let index = (getVarIndex name localVarList)
                    if index <= 3
                        then addToCurrentByteCodeSize 1
                        else addToCurrentByteCodeSize 2
                    return (codeWithoutPop ++ [(getAStoreByIndex index)])
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
            addToCurrentByteCodeSize 3
            return ([(Goto 0x0 0x0)] ++ codeForBlock)
        Nothing -> return []
    code_expr <- (generateCodeForIfElseStmtExpression expr 0x0 0x0)  -- current line number from monad to add branchoffset
    return (code_expr ++ code ++ code2)  -- goto muss irgendwo noch rein
-- Stmt Expr Stmt
generateCodeForStmt (StmtExprStmt stmtExpr) cp_infos = generateCodeForStmtExpr stmtExpr


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
    let codeWithoutPop =
            if isExprWithPopInstr expr2
                then init codeExpr2 -- delete last element (Pop instr.)
                else codeExpr2
    case codeExpr1 of
        [(PutField _ _)] -> do
            addToCurrentByteCodeSize 1
            return ([ALoad_0] ++ codeWithoutPop ++ codeExpr1)
        _ -> return (codeWithoutPop ++ codeExpr1)                                                                  
-- New
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr
-- Method call
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr              -- TODO ??

generateCodeForAssign :: Expression -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForAssign (TypedExpr expr _) = generateCodeForAssign expr
generateCodeForAssign (FieldVarExpr name) = do
    addToCurrentByteCodeSize 3
    return [(PutField 0x0 0x0)]                                                                                 -- TODO verweis auf cp mit name
generateCodeForAssign (LocalVarExpr name) = do
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes
    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList
    if var_type == BoolT || var_type == IntT || var_type == CharT
        then do
            let index = (getVarIndex name varList)
            if index <= 3
                then addToCurrentByteCodeSize 1
                else addToCurrentByteCodeSize 2
            return [(getIStoreByIndex index)]
        else do 
            let index = (getVarIndex name varList)
            if index <= 3
                then addToCurrentByteCodeSize 1
                else addToCurrentByteCodeSize 2
            return [(getAStoreByIndex index)]


generateCodeForMethodCallExpr :: MethodCallExpr -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) = do
    -- codeForExpr <- generateCodeForExpression expr  -- not needed since expr is always ThisExpr resulting in aload_0, because we only consider one class
    codeForExprs <- generateCodeForExpressions exprList
    return ([ALoad_0] ++ 
            codeForExprs ++ 
            [(InvokeVirtual 0x0 0x0),
            Pop])  -- needs to be deleted if method call expr is part of assignment or local var decl

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = return []                                                -- welcher Fall ist das?
generateCodeForExpression (SuperExpr) = return []                                               -- welcher Fall ist das?
generateCodeForExpression (FieldVarExpr name) = do
    addToCurrentByteCodeSize 4
    return [ALoad_0, (GetField 0x0 0x0)]                                                        -- TODO referenz auf cp über name
generateCodeForExpression (LocalVarExpr name) = do                                              
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes
    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList
    if var_type == BoolT || var_type == IntT || var_type == CharT
        then do
            let index = (getVarIndex name varList)
            if index <= 3
                then addToCurrentByteCodeSize 1
                else addToCurrentByteCodeSize 2
            return [(getILoadByIndex index)]
        else do 
            let index = (getVarIndex name varList)
            if index <= 3
                then addToCurrentByteCodeSize 1
                else addToCurrentByteCodeSize 2
            return [(getALoadByIndex (getVarIndex name varList))]
generateCodeForExpression (InstVarExpr expr name) = generateCodeForExpression expr              -- TODO: eigentlich nur relevant, wenn man mehrere Klassen hat?
generateCodeForExpression (UnaryOpExpr un_op expr) = do
    codeExpr <- generateCodeForExpression expr
    let (intExpr:[]) = codeExpr
    case un_op of
        UnaryPlus -> return codeExpr
        Not -> return []                                                                        -- TODO
        UnaryMinus -> do
            let instr = convertInstrToByteCode intExpr
            if length instr == 3
                then do 
                    let (codeInstr:intVal1:intVal2:[]) = instr
                        unsignedVal = (intVal1 `shiftL` 8) + intVal2
                    if unsignedVal > 32767  -- Check if value exceeds short range
                        then return [(SIPush ((-unsignedVal) `shiftR` 8) (-unsignedVal .&. 0xFF))]
                        else return [(BIPush (-unsignedVal))]
                else do
                    let (codeInstr:intVal:[]) = instr
                    return [(BIPush (-intVal))]
generateCodeForExpression (BinOpExpr expr1 bin_op expr2) = do
    codeExpr1 <- generateCodeForExpression expr1
    codeExpr2 <- generateCodeForExpression expr2
    case bin_op of
        Plus -> do 
            addToCurrentByteCodeSize 1
            return (codeExpr1 ++ codeExpr2 ++ [IAdd])
        Minus -> do
            addToCurrentByteCodeSize 1
            return (codeExpr1 ++ codeExpr2 ++ [ISub])
        Times -> do
            addToCurrentByteCodeSize 1
            return (codeExpr1 ++ codeExpr2 ++ [IMul])
        Divide -> do
            addToCurrentByteCodeSize 1
            return (codeExpr1 ++ codeExpr2 ++ [IDiv])
        --And -> return ([]) ??                                                                                     -- TODO
        --Or -> return ([]) ??
        Equal -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpNeq (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
        NotEqual -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpEq (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
        Less -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpGeq (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
        Greater -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpLeq (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
        LessEq -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpGt (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
        GreaterEq -> do
            byteCodeSize <- getCurrentByteCodeSize
            let code = codeExpr1 ++ 
                       codeExpr2 ++ 
                       [(If_ICmpLt (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 7) .&. 0xFF)), 
                        IConst_1, 
                        (Goto (((byteCodeSize + 7) `shiftR` 8) .&. 0xFF) ((byteCodeSize + 8) .&. 0xFF)), 
                        IConst_0]
            addToCurrentByteCodeSize 8
            return code
generateCodeForExpression (IntLitExpr intVal) = do
    if intVal > 127
        then do
            addToCurrentByteCodeSize 3 
            return [(SIPush ((intVal `shiftR` 8) .&. 0xFF) (intVal .&. 0xFF))]
        else do
            addToCurrentByteCodeSize 2 
            return [(BIPush intVal)]
generateCodeForExpression (BoolLitExpr bool) = case bool of
    True -> do 
        addToCurrentByteCodeSize 1
        return [(IConst_1)]
    False -> do 
        addToCurrentByteCodeSize 1
        return [(IConst_0)]
generateCodeForExpression (CharLitExpr (character:str)) = do
    addToCurrentByteCodeSize 2
    return [(BIPush (ord character))]
generateCodeForExpression (StringLitExpr _) = return []
generateCodeForExpression (Null) = do
    addToCurrentByteCodeSize 1
    return [(AConst_Null)]
generateCodeForExpression (StmtExprExpr stmtExpr) = generateCodeForStmtExpr stmtExpr


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewExpr (NewExpr newType args) = do
    code <- generateCodeForExpressions args
    return ([(New 0x0 0x0),
            (Dup)] ++
            code ++
            [(InvokeSpecial 0x0 0x0),
            (Pop)]) -- needs to be deleted if new expr is part of assignment or local var decl

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewType (NewType name) = return []

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpressions [] = return []
generateCodeForExpressions (expr:exprs) = do
    codeForExpr <- generateCodeForExpression expr
    codeForExprs <- generateCodeForExpressions exprs
    return (codeForExpr ++ codeForExprs)



----------------------------------------------------------------------
-- Helper function to check if new object is used in assignment or local var decl
isExprWithPopInstr :: Expression -> Bool
isExprWithPopInstr (TypedExpr (StmtExprExpr (TypedStmtExpr (NewExpression _) _)) _) = True
isExprWithPopInstr (TypedExpr (StmtExprExpr (TypedStmtExpr (MethodCall _) _ )) _) = True
isExprWithPopInstr _ = False

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