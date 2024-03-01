module CodeGenerator where

import Syntax
import ByteCodeInstr
import ClassFormat

-- TODO init
-- TODO change output to ints
-- TODO state monad for max stack size

-- Function to generate assembly code for init method
generateInitByteCode :: [CP_Info] -> [Int]
generateInitByteCode cp = byteCodeToOpCode_woArgs ALoad_0 ++ 
                          (byteCodeToOpCode_w2Args InvokeSpecial 0x00 0x00) ++  -- reference to init method??
                          (byteCodeToOpCode_woArgs Nop) ++
                          (byteCodeToOpCode_woArgs AConst_Null) ++
                          byteCodeToOpCode_woArgs Return

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> [CP_Info] -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForMethod (MethodDecl _ _ name params blockStmt) cp_infos =  -- TODO params auf Stack
  generateCodeForBlockStmt blockStmt cp_infos

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmt -> [CP_Info] -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] cp_infos = []
generateCodeForBlockStmt (stmt:stmts) cp_infos =
  generateCodeForStmt stmt ++ generateCodeForBlockStmt stmts cp_infos

-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForStmt (TypedStmt stmt _) = generateCodeForStmt stmt
generateCodeForStmt (ReturnStmt expr) = generateCodeForExpression expr
generateCodeForStmt (WhileStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (LocalVarDeclStmt thisType name) = [] -- name
generateCodeForStmt (LocalVarRefStmt thisType name expr) = [] ++ generateCodeForExpression expr  -- name ++ generateCodeForExpression expr
generateCodeForStmt (IfStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (IfElseStmt expr blockStmt maybeBlockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (StmtExprStmt stmtExpr) = generateCodeForStmtExpr stmtExpr



-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) =
  generateCodeForExpression expr1 ++ generateCodeForExpression expr2
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr


generateCodeForMethodCallExpr :: MethodCallExpr -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) =
    generateCodeForExpression expr ++ generateCodeForExpressions exprList

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> [Int] -- todo: -> ByteCode_Instrs
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
generateCodeForNewExpr :: NewExpr -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForNewExpr (NewExpr newType args) = []
  --"new " ++ generateCodeForNewType newType ++ ", " ++ -- Todo get index in constantpool
  --"dup, " ++
  --generateCodeForExpressions args ++
  --"invokespecial " -- Todo index of a Method ref

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForNewType (NewType name) = [] -- name  -- This is a simplistic approach

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> [Int] -- todo: -> ByteCode_Instrs
generateCodeForExpressions [] = []
generateCodeForExpressions (expr:exprs) =
  generateCodeForExpression expr ++ generateCodeForExpressions exprs

