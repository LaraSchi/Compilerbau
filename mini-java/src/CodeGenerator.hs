module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List (findIndex)
import Data.Char (ord)

import Control.Monad.State
import Debug.Trace

-- TODO init
-- TODO branchoffset 
-- TODO state monad:
    -- stack max size
    -- list of local vars to get index for store and load instructions
    -- list of istore_x
    -- list of iload_x
    -- line number for branchoffsets and stuff  -- vielleicht keine gute Idee, muss eine bessere her: 
                                                    -- Vielleicht immer nur die lens als Argumente als Branchoffset eingeben
                                                    -- wenn Liste fertig ist: über Liste iterieren und die Zahlen mit index + len ersetzen
    -- return type of method 

-- TODO: Idee für Iload und istore Zeug -> mit Namen der Variable im CP nach Index suchen und den einfach als index nehmen 
        -- -> geht nicht, local vars sind nicht im CP (aber man könnte Liste von vars in State monad führen)

-- TODO: unnötige Intr aus Datenstruktur schmeißen

-- State Monad
-- Define the state type
data GlobalVars = GlobalVars
    { maxSize :: Int -- Max Size
    , stack :: [Int] -- Stack  Todo change type in the list.
    } deriving (Show)

-- Define a type synonym for the state monad
type GlobalVarsMonad = State GlobalVars

-- Getter
-- Function to get the current integer from the state
getMaxSize :: GlobalVarsMonad Int
getMaxSize = gets maxSize

-- Function to get the current list from the state
getStack :: GlobalVarsMonad [Int]
getStack = gets stack


-- Function to add an integer to the max size
addToMaxSize :: Int -> GlobalVarsMonad ()
addToMaxSize x = modify (\s -> s { maxSize = maxSize s + x })

-- Function to add an element to the stack
addToStack :: Int -> GlobalVarsMonad ()
addToStack x = modify (\s -> s { stack = x : stack s })


-- Function to generate assembly code for init method
generateInitByteCode :: [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]  -- TODO: benutzen wenn im AST nicht gegeben
generateInitByteCode cp = return ([ALoad_0] ++
                        [(InvokeSpecial 0x00 (getIndexByDesc ("java/lang/Object" ++ "." ++ "<init>" ++ ":" ++ "()V") cp))] -- reference to init method TODO: what if there are multiple inits
                        ++ [Return])

startBuildGenCodeProcess :: MethodDecl -> [CP_Info] -> [ByteCodeInstrs]
startBuildGenCodeProcess m cp = do

    --runState (generateCodeForMethod m cp) (GlobalVars { maxSize = 0, stack = [] })
    let result = evalState (return []) (GlobalVars { maxSize = 0, stack = [] })
    let result = evalState (generateCodeForMethod m cp) (GlobalVars { maxSize = 0, stack = [] })

    result

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs]
generateCodeForMethod (MethodDecl visibility returnType name params stmt) cp_infos = do -- TODO params auf Stack
    addToMaxSize 1
    currentMaxSize <- getMaxSize
    traceShow ("Current Max Size: " ++ show currentMaxSize) $ return []
    --return []
    --generateCodeForBlockStmt stmt cp_infos
    

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmtList -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] cp_infos = return []
generateCodeForBlockStmt (stmt:stmts) cp_infos = do
    codeForStmt <- generateCodeForStmt stmt cp_infos
    codeForBlock <- generateCodeForBlockStmt stmts cp_infos
    return (codeForStmt ++ codeForBlock)
-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> [CP_Info] -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForStmt (TypedStmt stmt _) cp_infos = generateCodeForStmt stmt cp_infos
-- Return
generateCodeForStmt (ReturnStmt expr) cp_infos = 
    (generateCodeForExpression expr) {-++ 
    if monad.returnType == IntT || monad.returnType == BoolT || monad.returnType == CharT  -- TODO: get return type of methode from state monad
        then [IReturn]
        else if monad.returnType == VoidT
            then [Return]
            else [AReturn]-}
-- While
generateCodeForStmt (WhileStmt expr (Block blockStmt)) cp_infos = do
    code <- generateCodeForBlockStmt blockStmt cp_infos
    code_expr <- generateCodeForExpression expr  -- current line number from monad to add branchoffset
    return (code ++ code_expr)
-- LocalVar Decl 
generateCodeForStmt (LocalVarDeclStmt var_type name maybeExpr) cp_infos = 
    case maybeExpr of
        Just expr -> do
            codeForExpr <- generateCodeForExpression expr
            if var_type == BoolT || var_type == CharT || var_type == IntT
                then return (codeForExpr ++ [(IStore 0)])  -- TODO: richtigen Store Befehl aus State Monad nehmen
                else return (codeForExpr ++ [(AStore 0)])  -- TODO: richtigen Store Befehl aus State Monad nehmen
        Nothing -> return []  -- nur Decl ergibt keinen Binärcode, bei später Zuweisung ist es AssignmentStmt
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
generateCodeForStmtExpr :: StmtExpr -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
-- Assign Stmt
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) = do
    codeExpr1 <- generateCodeForExpression expr1
    codeExpr2 <- generateCodeForExpression expr2
    return (codeExpr1 ++ codeExpr2)
-- New
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr  -- TODO ??
-- Method call
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr  -- TODO ??


generateCodeForMethodCallExpr :: MethodCallExpr -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) = do
    --generateCodeForExpression expr ++ generateCodeForExpressions exprList
    codeForExpr <- generateCodeForExpression expr
    codeForExprs <- generateCodeForExpressions exprList
    return (codeForExpr ++ codeForExprs)

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = return []
generateCodeForExpression (SuperExpr) = return []
generateCodeForExpression (FieldVarExpr name) = return [(ILoad 0)] -- dummy TODO: search with name for the index of this variable
generateCodeForExpression (LocalVarExpr name) = return [(ILoad 0)] -- dummy TODO: search with name for the index of this variable
generateCodeForExpression (InstVarExpr expr name) = generateCodeForExpression expr -- ++ name  im neuen Parser nicht vorhanden
generateCodeForExpression (UnaryOpExpr un_op expr) = generateCodeForExpression expr
generateCodeForExpression (BinOpExpr expr1 _ expr2) = do
-- generateCodeForExpression expr1 ++ generateCodeForExpression expr2
    codeExpr1 <- generateCodeForExpression expr1
    codeExpr2 <- generateCodeForExpression expr2
    return (codeExpr1 ++ codeExpr2)
generateCodeForExpression (IntLitExpr intVal) = return [(BIPush intVal)]  -- "iconst_" ++ show intVal
generateCodeForExpression (BoolLitExpr bool) = case bool of
    True -> return [(IConst_1)]
    False -> return [(IConst_0)]
generateCodeForExpression (CharLitExpr (character:str)) = return [(BIPush (ord character))]
generateCodeForExpression (StringLitExpr _) = return []  -- noch nicht unterstützt
generateCodeForExpression (Null) = return [(AConst_Null)]
generateCodeForExpression (StmtExprExpr stmtExpr) = generateCodeForStmtExpr stmtExpr


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForNewExpr (NewExpr newType args) = return []
    --"new " ++ generateCodeForNewType newType ++ ", " ++ -- Todo get index in constantpool
    --"dup, " ++
    --generateCodeForExpressions args ++
    --"invokespecial " -- Todo index of a Method ref

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForNewType (NewType name) = return [] -- name  -- This is a simplistic approach

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> GlobalVarsMonad [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForExpressions [] = return []
generateCodeForExpressions (expr:exprs) = do
    codeForExpr <- generateCodeForExpression expr
    codeForExprs <- generateCodeForExpressions exprs
    return (codeForExpr ++ codeForExprs)
----------------------------------------------------------------------
-- Helper function for constant pool

getIndexByDesc :: String -> [CP_Info] -> Int
getIndexByDesc descriptor cpList =
    case findIndex (\cpInfo -> desc cpInfo == descriptor) cpList of
        Just idx -> idx + 1 -- Constant pool begins at 1.
        Nothing  -> -1