module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List (findIndex)
import Data.Char (ord)

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
    

-- Function to generate assembly code for init method
generateInitByteCode :: [CP_Info] -> [ByteCodeInstrs]  -- TODO: benutzen wenn im AST nicht gegeben
generateInitByteCode cp = [ALoad_0] ++
                        [(InvokeSpecial 0x00 (getIndexByDesc ("java/lang/Object" ++ "." ++ "<init>" ++ ":" ++ "()V") cp))] -- reference to init method TODO: what if there are multiple inits
                        ++ [Return]

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> [ByteCodeInstrs]
generateCodeForMethod (MethodDecl visibility returnType name params (Block blockStmt)) cp_infos =  -- TODO params auf Stack
    generateCodeForBlockStmt blockStmt cp_infos 
    

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmtList -> [CP_Info] -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] cp_infos = []
generateCodeForBlockStmt (stmt:stmts) cp_infos =
    generateCodeForStmt stmt cp_infos ++ generateCodeForBlockStmt stmts cp_infos

-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> [CP_Info] -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
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
generateCodeForStmt (WhileStmt expr (Block blockStmt)) cp_infos = 
    let code = generateCodeForBlockStmt blockStmt cp_infos
        code_expr = generateCodeForExpression expr  -- current line number from monad to add branchoffset
    in code ++ code_expr
-- LocalVar Decl 
generateCodeForStmt (LocalVarDeclStmt var_type name maybeExpr) cp_infos = 
    case maybeExpr of
        Just expr -> generateCodeForExpression expr ++
            if var_type == BoolT || var_type == CharT || var_type == IntT
                then [(IStore 0)]  -- TODO: richtigen Store Befehl aus State Monad nehmen
                else [(AStore 0)]  -- TODO: richtigen Store Befehl aus State Monad nehmen 
        Nothing -> []  -- nur Decl ergibt keinen Binärcode, bei später Zuweisung ist es AssignmentStmt 
-- If Else  !! nur ifcmp* werden verwendet; javac hat Sonderinstruktionen wenn Vergleich mit 0 durchgeführt wird, aber unnötig
generateCodeForStmt (IfElseStmt expr (Block blockStmt) maybeBlockStmt) cp_infos = 
    let code = generateCodeForBlockStmt blockStmt cp_infos
        code2 = case maybeBlockStmt of
            Just (Block block) -> [(Goto 0x0 0x0)] ++ generateCodeForBlockStmt block cp_infos
            Nothing -> []
        code_expr = (generateCodeForIfElseStmtExpression expr 0x0 0x0)  -- current line number from monad to add branchoffset
    in code_expr ++ code ++ code2  -- goto muss irgendwo noch rein
-- Stmt Expr Stmt
generateCodeForStmt (StmtExprStmt stmtExpr) cp_infos = generateCodeForStmtExpr stmtExpr


{- Function to build byte code for if else statement
    currently only works with if_icmp* (so int, char and boolean) 
    -> need type of expr1 or expr2 to choose between if_icmp* and if_acmp*
-}
generateCodeForIfElseStmtExpression :: Expression -> Int -> Int -> [ByteCodeInstrs] 
generateCodeForIfElseStmtExpression (BinOpExpr expr1 binop expr2) len1 len2 = 
    let if_code = case binop of
            Equal -> [(If_ICmpNeq 0x0 0x0)]
            NotEqual -> [(If_ICmpEq) 0x0 0x0]
            Less -> [(If_ICmpGeq) 0x0 0x0]
            Greater -> [(If_ICmpLeq) 0x0 0x0]
            LessEq -> [(If_ICmpGt) 0x0 0x0]
            GreaterEq -> [(If_ICmpLt) 0x0 0x0]
    in (generateCodeForExpression expr1) ++ (generateCodeForExpression expr2) ++ if_code 

-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
-- Assign Stmt
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) =
    (generateCodeForExpression expr1) ++ (generateCodeForExpression expr2)
-- New
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr  -- TODO ??
-- Method call
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr  -- TODO ??


generateCodeForMethodCallExpr :: MethodCallExpr -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) =
    generateCodeForExpression expr ++ generateCodeForExpressions exprList

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = []
generateCodeForExpression (SuperExpr) = []
generateCodeForExpression (FieldVarExpr name) = [(ILoad 0)] -- dummy TODO: search with name for the index of this variable
generateCodeForExpression (LocalVarExpr name) = [(ILoad 0)] -- dummy TODO: search with name for the index of this variable
generateCodeForExpression (InstVarExpr expr name) = generateCodeForExpression expr -- ++ name  im neuen Parser nicht vorhanden
generateCodeForExpression (UnaryOpExpr un_op expr) = generateCodeForExpression expr
generateCodeForExpression (BinOpExpr expr1 _ expr2) = generateCodeForExpression expr1 ++ generateCodeForExpression expr2
generateCodeForExpression (IntLitExpr intVal) = [(BIPush intVal)]  -- "iconst_" ++ show intVal
generateCodeForExpression (BoolLitExpr bool) = case bool of
    True -> [(IConst_1)]
    False -> [(IConst_0)]
generateCodeForExpression (CharLitExpr (character:str)) = [(BIPush (ord character))] 
generateCodeForExpression (StringLitExpr _) = []  -- noch nicht unterstützt
generateCodeForExpression (Null) = [(AConst_Null)]
generateCodeForExpression (StmtExprExpr stmtExpr) = generateCodeForStmtExpr stmtExpr


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForNewExpr (NewExpr newType args) = []
    --"new " ++ generateCodeForNewType newType ++ ", " ++ -- Todo get index in constantpool
    --"dup, " ++
    --generateCodeForExpressions args ++
    --"invokespecial " -- Todo index of a Method ref

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForNewType (NewType name) = [] -- name  -- This is a simplistic approach

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForExpressions [] = []
generateCodeForExpressions (expr:exprs) =
    generateCodeForExpression expr ++ generateCodeForExpressions exprs

----------------------------------------------------------------------
-- Helper function for constant pool

getIndexByDesc :: String -> [CP_Info] -> Int
getIndexByDesc descriptor cpList =
    case findIndex (\cpInfo -> desc cpInfo == descriptor) cpList of
        Just idx -> idx + 1 -- Constant pool begins at 1.
        Nothing  -> -1