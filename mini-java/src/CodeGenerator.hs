module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List
import Data.Char (ord)
import Data.Bits
import Control.Monad.State
import ConstPoolGen
import Debug.Trace

-- TODO max stack size berechnen
-- TODO branchoffset 
-- TODO unary: Not
-- TODO binary: &&, ||
-- TODO CharLitExpr in Char
-- TODO: unnötige Instr aus Datenstruktur schmeißen


----------------------------------------------------------------------
-- State Monad
-- Define the state type
data GlobalVars = GlobalVars
    { maxStackSize :: Int               -- max stack size
    , currentStackSize :: Int           -- current stack size
    , currentByteCodeSize :: Int        -- current byte code array size
    , localVars :: [String]             -- list of defined local variables to choose index for iload, istore, ...
    , typesOfLocalVars :: [Type]        -- list of the local variable types
    , returnType :: Type                -- method return type
    , className :: String               -- name of class
    , methodDeklr :: [MethodDecl]       -- List of MethodDeklerations
    } deriving (Show)

-- Define a type synonym for the state monad
type GlobalVarsMonad = State GlobalVars

-- Getter functions
getMaxSize :: GlobalVarsMonad Int
getMaxSize = gets maxStackSize

getCurrentStackSize :: GlobalVarsMonad Int
getCurrentStackSize = gets currentStackSize

getCurrentByteCodeSize :: GlobalVarsMonad Int
getCurrentByteCodeSize = gets currentByteCodeSize

getReturnType :: GlobalVarsMonad Type
getReturnType = gets returnType

getClassName :: GlobalVarsMonad String
getClassName = gets className

getLocalVars :: GlobalVarsMonad [String]
getLocalVars = gets localVars

getLocalVarTypes :: GlobalVarsMonad [Type]
getLocalVarTypes = gets typesOfLocalVars

getMethodDeklr :: GlobalVarsMonad [MethodDecl]
getMethodDeklr = gets methodDeklr

-- Functions to count the integers in the monad
addToMaxStackSize :: Int -> GlobalVarsMonad ()
addToMaxStackSize x = modify (\s -> s { maxStackSize = maxStackSize s + x })

addToCurrentStackSize :: Int -> GlobalVarsMonad ()
addToCurrentStackSize x = modify (\s -> s { currentStackSize = currentStackSize s + x })

addToCurrentByteCodeSize :: Int -> GlobalVarsMonad ()
addToCurrentByteCodeSize x = modify (\s -> s { currentByteCodeSize = currentByteCodeSize s + x })


-- Function to set return type of method
setReturnType :: Type -> GlobalVarsMonad ()
setReturnType x = modify (\s -> s { returnType = x })                                         


-- Function to add an element to the lists of the monad
addToLocalVars :: String -> GlobalVarsMonad ()
addToLocalVars x = modify (\s -> s { localVars = localVars s ++ [x] })

addToLocalVarTypes :: Type -> GlobalVarsMonad ()
addToLocalVarTypes x = modify (\s -> s { typesOfLocalVars = typesOfLocalVars s ++ [x] })



----------------------------------------------------------------------
-- Functions to generate Byte Code
startBuildGenCodeProcess :: MethodDecl -> [CP_Info] -> String -> [MethodDecl] -> ([ByteCodeInstrs], Int)
startBuildGenCodeProcess m cp className methods =
    let (result, finalState) = runState (generateCodeForMethod m cp) initialState
    in (result, maxStackSize finalState)
    where
    initialState = GlobalVars { maxStackSize = 0, currentStackSize = 0, currentByteCodeSize = 0, 
                                localVars = [], typesOfLocalVars = [], returnType = VoidT, className = className, methodDeklr=methods  }
                        

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethod (MethodDecl visibility retType name params stmt) cp_infos = do
    className <- getClassName
    let initCode =
            if name == className
                then let deskr = ("java/lang/Object" ++ "." ++ "<init>" ++ ":()V")  -- cp ref to java/lang/Object."<init>":()V
                         idx = getIndexByDesc deskr cp_infos 
                     in [(ALoad_0), (InvokeSpecial ((idx `shiftR` 8) .&. 0xFF) (idx .&. 0xFF))]
                else []
    addParamsToMonad params
    if retType == VoidT
        then do
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
generateCodeForStmt (TypedStmt stmt _) cp_infos =  generateCodeForStmt stmt cp_infos
generateCodeForStmt (Block stmts) cp_infos = generateCodeForBlockStmt stmts cp_infos
-- Return
generateCodeForStmt (ReturnStmt expr) cp_infos = do
    codeForExpr <- (generateCodeForExpression expr cp_infos)
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
generateCodeForStmt (WhileStmt expr stmt) cp_infos = do                               -- TODO
    code <- generateCodeForStmt stmt cp_infos
    code_expr <- generateCodeForExpression expr cp_infos
    return (code ++ code_expr)

-- LocalVar Decl 
generateCodeForStmt (LocalVarDeclStmt var_type name maybeExpr) cp_infos = 
    case maybeExpr of
        Just expr -> do
            addToLocalVars name
            addToLocalVarTypes var_type
            localVarList <- getLocalVars
            codeForExpr <- generateCodeForExpression expr cp_infos
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
-- If Else: nur ifcmp* werden verwendet; javac hat Sonderinstruktionen 
--          wenn Vergleich mit 0 durchgeführt wird, aber unnötig
generateCodeForStmt (IfElseStmt expr stmt maybeBlockStmt) cp_infos = do
    case maybeBlockStmt of
        Just stmt2 -> do
            case expr of
                (TypedExpr (BinOpExpr expr1 bin_op expr2) _) -> do
                    code2 <- generateCodeForStmt stmt2 cp_infos
                    code1 <- generateCodeForStmt stmt cp_infos
                    code <- (generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos code1 code2) 
                    return (code)
                _ -> do
                    -- Ifeq things
                    return []
        Nothing -> do
            case expr of
                (TypedExpr (BinOpExpr expr1 bin_op expr2) _) -> do
                    code1 <- generateCodeForStmt stmt cp_infos
                    code <- (generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos code1 [])
                    return (code)
                _ -> do
                    -- Ifeq things
                    return []


-- Stmt Expr Stmt
generateCodeForStmt (StmtExprStmt stmtExpr) cp_infos = generateCodeForStmtExpr stmtExpr cp_infos
generateCodeForStmt (Print name) cp_infos = return [] -- Print Statement


-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) cp_infos = generateCodeForStmtExpr stmtExpr cp_infos
-- Assign Stmt
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) cp_infos = do
    codeExpr1 <- generateCodeForExpression expr1 cp_infos
    codeExpr2 <- generateCodeForExpression expr2 cp_infos
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
generateCodeForStmtExpr (NewExpression expr) cp_infos = generateCodeForNewExpr expr cp_infos
-- Method call
generateCodeForStmtExpr (MethodCall methodCallExpr) cp_infos = generateCodeForMethodCallExpr methodCallExpr cp_infos

generateCodeForAssign :: Expression -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForAssign (TypedExpr expr _) cp_infos = do
    generateCodeForAssign expr cp_infos
    generateCodeForExpression expr cp_infos
generateCodeForAssign (FieldVarExpr name) cp_infos = do
    addToCurrentByteCodeSize 3
    className <- getClassName
    let search_string1 = className ++ "." ++ name ++ ":I"
        search_string2 = className ++ "." ++ name ++ ":Z"
        search_string3 = className ++ "." ++ name ++ ":C"
        search_string4 = className ++ "." ++ name ++ ":" ++ className
        idx1 = getIndexByDesc search_string1 cp_infos
        idx2 = getIndexByDesc search_string2 cp_infos
        idx3 = getIndexByDesc search_string3 cp_infos
        idx4 = getIndexByDesc search_string4 cp_infos
        index = 
            if idx1 /= (-1)
                then idx1
                else if idx2 /= (-1)
                    then idx2
                    else if idx3 /= (-1)
                        then idx3
                        else idx4
    return [(PutField ((index `shiftR` 8) .&. 0xFF) (index .&. 0xFF))]          -- Verweis auf cp mit "classname.fieldname:Type" (int -> I, boolean -> Z, char -> C, classname -> classname)
generateCodeForAssign (LocalVarExpr name) cp_infos = do
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes

    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList -- Todo delete this comment


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


generateCodeForMethodCallExpr :: MethodCallExpr -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) cp_infos =  do
    -- codeForExpr <- generateCodeForExpression expr  -- not needed since expr is always ThisExpr resulting in aload_0, because we only consider one class
    codeForExprs <- generateCodeForExpressions exprList cp_infos
    className <- getClassName
    decls <- getMethodDeklr
    let methodRefs = filter (\(MethodDecl _ _ methodName prameters _) -> name == methodName && (areInputTypesCorrect exprList prameters))  decls
    case methodRefs of
        [MethodDecl _ thisType methodName parameters _] -> do
             let methodType = ("(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString thisType)
             let deskr = className ++ "." ++ name ++ ":" ++ methodType
             let idx = getIndexByDesc deskr cp_infos
             return ([ALoad_0] ++
                codeForExprs ++
                [(InvokeVirtual ((idx `shiftR` 8) .&. 0xFF) (idx .&. 0xFF)), -- ((index `shiftR` 8) .&. 0xFF) (index .&. 0xFF)),  -- Verweis auf methodref in cp mit "classname.methodname:(paramtypes)returntype" e.g test.add:(II)I
                Pop])  -- needs to be deleted if method call expr is part of assignment or local var decl


        _:(_:_) -> trace ("The following methods clash due to the same erasure: " ++
                           concatMap (\(MethodDecl _ thisType methodName parameters _) ->
                               let methodType = "(" ++ intercalate "" (concatMap getInputType parameters) ++ ")" ++ typeToString thisType
                               in "Name: " ++ methodName ++ ", Method Type: " ++ methodType ++ "\n"
                           ) methodRefs
                       ) $ return []



-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpression (ThisExpr) cp_infos = return []                                                
generateCodeForExpression (SuperExpr) cp_infos = return []                                               
generateCodeForExpression (TypedExpr expr _) cp_infos = generateCodeForExpression expr cp_infos
generateCodeForExpression (FieldVarExpr name) cp_infos = do
    addToCurrentByteCodeSize 4
    className <- getClassName
    let search_string1 = className ++ "." ++ name ++ ":I"
        search_string2 = className ++ "." ++ name ++ ":Z"
        search_string3 = className ++ "." ++ name ++ ":C"
        search_string4 = className ++ "." ++ name ++ ":" ++ className
        idx1 = getIndexByDesc search_string1 cp_infos
        idx2 = getIndexByDesc search_string2 cp_infos
        idx3 = getIndexByDesc search_string3 cp_infos
        idx4 = getIndexByDesc search_string4 cp_infos
        index = 
            if idx1 /= (-1)
                then idx1
                else if idx2 /= (-1)
                    then idx2
                    else if idx3 /= (-1)
                        then idx3
                        else idx4
    return [ALoad_0, (GetField ((index `shiftR` 8) .&. 0xFF) (index .&. 0xFF))] -- Verweis auf cp mit "classname.fieldname:Type" (int -> I, boolean -> Z, char -> C, classname -> classname)
generateCodeForExpression (LocalVarExpr name) cp_infos = do                                              
    varList <- getLocalVars
    varTypeList <- getLocalVarTypes

    let var_type = getTypeFromIndex (getVarIndex name varList) varTypeList -- Todo delete this comment
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
generateCodeForExpression (InstVarExpr expr name) cp_infos = generateCodeForExpression expr cp_infos        -- TODO: eigentlich nur relevant, wenn man mehrere Klassen hat?
generateCodeForExpression (UnaryOpExpr un_op expr) cp_infos = do
    codeExpr <- generateCodeForExpression expr cp_infos
    let (intExpr:[]) = codeExpr
    case un_op of
        UnaryPlus -> return codeExpr
        Not -> return [] -- not implemented
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
generateCodeForExpression (BinOpExpr expr1 bin_op expr2) cp_infos = do
    case bin_op of
        Equal -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        NotEqual -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        Less  -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        Greater  -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        LessEq  -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        GreaterEq  -> do 
            addToCurrentByteCodeSize 2
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [IConst_1] [IConst_0]
            return (code)
        _ -> do 
            code <- generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos [] []
            return (code)
    
    
generateCodeForExpression (IntLitExpr intVal) cp_infos = do
    if intVal > 127
        then do
            addToCurrentByteCodeSize 3 
            return [(SIPush ((intVal `shiftR` 8) .&. 0xFF) (intVal .&. 0xFF))]
        else do
            addToCurrentByteCodeSize 2 
            return [(BIPush intVal)]
generateCodeForExpression (BoolLitExpr bool) cp_infos = case bool of
    True -> do 
        addToCurrentByteCodeSize 1
        return [(IConst_1)]
    False -> do 
        addToCurrentByteCodeSize 1
        return [(IConst_0)]
generateCodeForExpression (CharLitExpr (character:str)) cp_infos = do
    addToCurrentByteCodeSize 2
    return [(BIPush (ord character))]
generateCodeForExpression (StringLitExpr _) cp_infos = return []
generateCodeForExpression (Null) cp_infos = do
    addToCurrentByteCodeSize 1
    return [(AConst_Null)]
generateCodeForExpression (StmtExprExpr stmtExpr) cp_infos = generateCodeForStmtExpr stmtExpr cp_infos


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewExpr (NewExpr newType args) cp_infos = do
    code <- generateCodeForExpressions args cp_infos
    className <- getClassName
    let idx_method_ref = getIndexByDesc (className ++ ".<init>:()V") cp_infos
        idx_class_ref = getIndexByDesc className cp_infos
    return ([(New ((idx_class_ref `shiftR` 8) .&. 0xFF) (idx_class_ref .&. 0xFF)),  -- Verweis auf class_info mit desc classname
            (Dup)] ++
            code ++
            [(InvokeSpecial ((idx_method_ref `shiftR` 8) .&. 0xFF) (idx_method_ref .&. 0xFF)),  -- Verweis auf methodref "classname.<init>:()V"
            (Pop)]) -- needs to be deleted if new expr is part of assignment or local var decl

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForNewType (NewType name) = return []

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForExpressions [] cp_infos = return []
generateCodeForExpressions (expr:exprs) cp_infos = do
    codeForExpr <- generateCodeForExpression expr cp_infos
    codeForExprs <- generateCodeForExpressions exprs cp_infos
    return (codeForExpr ++ codeForExprs)


generateCodeForBinOpExpr :: Expression -> [CP_Info] -> [ByteCodeInstrs] -> [ByteCodeInstrs] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForBinOpExpr (BinOpExpr expr1 bin_op expr2) cp_infos c1 c2 = do
    codeExpr1 <- generateCodeForExpression expr1 cp_infos
    codeExpr2 <- generateCodeForExpression expr2 cp_infos
    let codeBlock1 = c1
        codeBlock2 = c2
        lenCodeBlock1 = case c1 of
            [] -> 0
            _ -> length (convertToByteCode c1)
        lenCodeBlock2 = case c2 of
            [] -> 0
            _ -> length (convertToByteCode c2)
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
        Syntax.And -> return ([])  -- not implemented
        Syntax.Or -> return ([])  -- not implemented
        Equal -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpNeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpNeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code
        NotEqual -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpEq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpEq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code
        Less -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpGeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpGeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code
        Greater -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpLeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpLeq (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code
        LessEq -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpGt (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpGt (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code
        GreaterEq -> do
            if lenCodeBlock2 /= 0
                then do 
                    addToCurrentByteCodeSize 6  -- ifcmp and goto
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpLt (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1 ++
                                [(Goto (((byteCodeSize) `shiftR` 8) .&. 0xFF) ((byteCodeSize) .&. 0xFF))] ++ 
                                codeBlock2)
                    return code
                else do
                    addToCurrentByteCodeSize 3  -- ifcmp
                    byteCodeSize <- getCurrentByteCodeSize
                    let code = (codeExpr1 ++ 
                                codeExpr2 ++ 
                                [(If_ICmpLt (((byteCodeSize - lenCodeBlock2) `shiftR` 8) .&. 0xFF) ((byteCodeSize - lenCodeBlock2) .&. 0xFF))] ++ 
                                codeBlock1)
                    return code


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