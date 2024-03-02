module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat
import Data.List (findIndex)

-- TODO init
-- TODO change output to ints
-- TODO state monad for max stack size

-- Function to generate assembly code for init method
generateInitByteCode :: [CP_Info] -> [ByteCodeInstrs]
generateInitByteCode cp = byteCodeToOpCode_woArgs ALoad_0 ++
                        (byteCodeToOpCode_w2Args InvokeSpecial 0x00 0x01)
                        ++ getIndexByDesc ("java/lang/Object" ++ "." ++ "<init>" ++ ":" ++ "()V") cp-- reference to init method
                        ++ byteCodeToOpCode_woArgs Return

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForMethod (MethodDecl visibility returnType name params blockStmt) cp_infos =  -- TODO params auf Stack
    byteCodeToOpCode_woArgs ALoad_0 ++  -- every method starts with this instruction
    generateCodeForBlockStmt blockStmt cp_infos ++
    if returnType == IntT || returnType == BoolT || returnType == CharT
        then byteCodeToOpCode_woArgs IReturn
        else if returnType == VoidT
            then byteCodeToOpCode_woArgs (Vier Return)
            else byteCodeToOpCode_woArgs AReturn

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmt -> [CP_Info] -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] cp_infos = []
generateCodeForBlockStmt (stmt:stmts) cp_infos =
    generateCodeForStmt stmt ++ generateCodeForBlockStmt stmts cp_infos

-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForStmt (TypedStmt stmt _) = generateCodeForStmt stmt
generateCodeForStmt (ReturnStmt expr) = []  -- ireturn if int, char or boolean (bei Klassen areturn) -> bei methodcode am Ende hinzufügen?
generateCodeForStmt (WhileStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (LocalVarDeclStmt thisType name) = []  -- ist das nur die Deklaration ohne Zuweisung? da passiert nämlich im Code nichts
generateCodeForStmt (LocalVarRefStmt thisType name expr) = 
    if thisType == BoolT
        then if expr == (BoolLitExpr True) 
            then byteCodeToOpCode_woArgs IConst_1 ++ byteCodeToOpCode_woArgs IStore_1  -- istore nur bei erster variable ... woher weiß ich, wie viele ich schon habe
            else if expr == (BoolLitExpr False) 
                then byteCodeToOpCode_woArgs IConst_0 ++ byteCodeToOpCode_woArgs IStore_1  -- istore nur bei erster variable ... woher weiß ich, wie viele ich schon habe
                else [] -- dummy
        else [] --dummy
generateCodeForStmt (IfStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (IfElseStmt expr blockStmt maybeBlockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (StmtExprStmt stmtExpr) = generateCodeForStmtExpr stmtExpr




-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) =
    generateCodeForExpression expr1 ++ generateCodeForExpression expr2
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr


generateCodeForMethodCallExpr :: MethodCallExpr -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) =
    generateCodeForExpression expr ++ generateCodeForExpressions exprList

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> [ByteCodeInstrs] -- todo: -> ByteCode_Instrs
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = []
generateCodeForExpression (SuperExpr) = []
generateCodeForExpression (IdentifierExpr name) = []  -- This is a simplistic approach
generateCodeForExpression (InstVar expr name) = generateCodeForExpression expr -- ++ name
generateCodeForExpression (UnaryOpExpr _ expr) = generateCodeForExpression expr
generateCodeForExpression (BinOpExpr expr1 _ expr2) = generateCodeForExpression expr1 ++ generateCodeForExpression expr2
generateCodeForExpression (IntLitExpr intVal) = []  -- "iconst_" ++ show intVal
generateCodeForExpression (BoolLitExpr _) = []
generateCodeForExpression (CharLitExpr _) = []
generateCodeForExpression (StringLitExpr _) = []
generateCodeForExpression (Null) = []
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

getIndexByDesc :: String -> [CP_Info] -> [Int]
getIndexByDesc descriptor cpList =
    case findIndex (\cpInfo -> desc cpInfo == descriptor) cpList of
        Just idx -> [idx + 1] -- Constant pool begins at 1.
        Nothing  -> [-1]