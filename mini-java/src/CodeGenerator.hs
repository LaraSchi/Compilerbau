module CodeGenerator where

import Syntax
import ByteCodeInstr

-- TODO init
-- TODO change output to ByteCode_Instrs

-- Function to generate assembly code for a Method
generateCodeForMethod :: MethodDecl -> String -- todo: -> ByteCode_Instrs
generateCodeForMethod (MethodDecl _ _ name params blockStmt) =
  generateCodeForBlockStmt blockStmt ++ ", "

-- Function to generate assembly code for BlockStmt
generateCodeForBlockStmt :: BlockStmt -> String -- todo: -> ByteCode_Instrs
generateCodeForBlockStmt [] = ""
generateCodeForBlockStmt (stmt:stmts) =
  generateCodeForStmt stmt ++ generateCodeForBlockStmt stmts

-- Function to generate assembly code for Stmt
generateCodeForStmt :: Stmt -> String -- todo: -> ByteCode_Instrs
generateCodeForStmt (TypedStmt stmt _) = generateCodeForStmt stmt
generateCodeForStmt (ReturnStmt expr) = generateCodeForExpression expr ++ "areturn, "
generateCodeForStmt (WhileStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (LocalVarDeclStmt thisType name) = name
generateCodeForStmt (LocalVarRefStmt thisType name expr) = name ++ generateCodeForExpression expr
generateCodeForStmt (IfStmt expr blockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (IfElseStmt expr blockStmt maybeBlockStmt) = generateCodeForExpression expr --Todo Blockstatement?
generateCodeForStmt (StmtExprStmt stmtExpr) = generateCodeForStmtExpr stmtExpr



-- Function to generate assembly code for StmtExpr
generateCodeForStmtExpr :: StmtExpr -> String -- todo: -> ByteCode_Instrs
generateCodeForStmtExpr (TypedStmtExpr stmtExpr _) = generateCodeForStmtExpr stmtExpr
generateCodeForStmtExpr (AssignmentStmt expr1 expr2) =
  generateCodeForExpression expr1 ++ generateCodeForExpression expr2 ++ "putfield, "
generateCodeForStmtExpr (NewExpression expr) = generateCodeForNewExpr expr
generateCodeForStmtExpr (MethodCall methodCallExpr) = generateCodeForMethodCallExpr methodCallExpr


generateCodeForMethodCallExpr :: MethodCallExpr -> String -- todo: -> ByteCode_Instrs
generateCodeForMethodCallExpr (MethodCallExpr expr name exprList) =
    generateCodeForExpression expr ++ generateCodeForExpressions exprList

-- Function to generate assembly code for Expression
generateCodeForExpression :: Expression -> String -- todo: -> ByteCode_Instrs
generateCodeForExpression (TypedExpr expr _) = generateCodeForExpression expr
generateCodeForExpression (ThisExpr) = "Dont' know what to do here"
generateCodeForExpression (SuperExpr) = "Dont' know what to do here"
generateCodeForExpression (IdentifierExpr name) = "aload " ++ name ++ ", "  -- This is a simplistic approach
generateCodeForExpression (InstVar expr name) = generateCodeForExpression expr ++ name
generateCodeForExpression (UnaryOpExpr _ expr) = generateCodeForExpression expr
generateCodeForExpression (BinOpExpr expr1 _ expr2) = generateCodeForExpression expr1 ++ generateCodeForExpression expr2
generateCodeForExpression (IntLitExpr intVal) = "iconst_" ++ show intVal
generateCodeForExpression (BoolLitExpr _) = "Dont' know what to do here"
generateCodeForExpression (CharLitExpr _) = "Dont' know what to do here"
generateCodeForExpression (StringLitExpr _) = "Dont' know what to do here"
generateCodeForExpression (Null) = "Dont' know what to do here"
generateCodeForExpression (StmtExprExpr stmtExpr) = generateCodeForStmtExpr stmtExpr


-- Function to generate assembly code for NewExpr
generateCodeForNewExpr :: NewExpr -> String -- todo: -> ByteCode_Instrs
generateCodeForNewExpr (NewExpr newType args) =
  "new " ++ generateCodeForNewType newType ++ ", " ++ -- Todo get index in constantpool
  "dup, " ++
  generateCodeForExpressions args ++
  "invokespecial " -- Todo index of a Method ref

-- Function to generate assembly code for NewType
generateCodeForNewType :: NewType -> String -- todo: -> ByteCode_Instrs
generateCodeForNewType (NewType name) = name  -- This is a simplistic approach

-- Function to generate assembly code for Expressions
generateCodeForExpressions :: [Expression] -> String -- todo: -> ByteCode_Instrs
generateCodeForExpressions [] = ""
generateCodeForExpressions (expr:exprs) =
  generateCodeForExpression expr ++ generateCodeForExpressions exprs

