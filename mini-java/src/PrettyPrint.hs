module PrettyPrint where

import Syntax
import Data.List

-- ANSI

greenColor :: String
greenColor = "\x1b[32m"

redColor :: String
redColor = "\x1b[31m"

blueColor :: String
blueColor = "\x1b[34m"

resetColor :: String
resetColor = "\x1b[0m"

dimColor :: String
dimColor = "\x1b[2m"


-- Pretty Printer für Programme
prettyPrintProgram :: Program -> String
prettyPrintProgram (Program cls typed) =
    prettyPrintClass cls ++ " (" ++ greenColor ++ show typed ++ resetColor ++ ")"

-- Pretty Printer für Klassen
prettyPrintClass :: Class -> String
prettyPrintClass (Class newType fields methods) =
    "class " ++ prettyPrintNewType newType ++ " {\n" ++
    concatMap prettyPrintField fields ++
    concatMap prettyPrintMethodDecl methods ++
    "}\n"

-- Pretty Printer für Felder
prettyPrintField :: Field -> String
prettyPrintField (FieldDecl typ name expr) =
    "    " ++ prettyPrintType typ ++ " " ++ name ++ maybeExpr expr ++ ";\n"
    where
        maybeExpr :: Maybe Expression -> String
        maybeExpr Nothing = ""
        maybeExpr (Just e) = " = " ++ prettyPrintExpression e

-- Pretty Printer für Methodendeklarationen
prettyPrintMethodDecl :: MethodDecl -> String
prettyPrintMethodDecl (MethodDecl vis typ name params stmt) =
    "    " ++ prettyPrintVisibility vis ++ " " ++
    prettyPrintType typ ++ " " ++ name ++ "(" ++
    commaSep (map prettyPrintParameter params) ++ ") " ++
    prettyPrintStmt stmt ++
    "\n"

-- Pretty Printer für Parameter
prettyPrintParameter :: Parameter -> String
prettyPrintParameter (Parameter typ name) =
    prettyPrintType typ ++ " " ++ name

-- Pretty Printer für Statements
prettyPrintStmt :: Stmt -> String
prettyPrintStmt (TypedStmt stmt@(Block _) typ) =
     "(" ++prettyPrintStmt stmt ++ blueColor ++ "  :: " ++ prettyPrintType typ ++ resetColor ++ ")"
prettyPrintStmt (TypedStmt stmt typ) =
    prettyPrintStmt stmt ++ greenColor ++ "  (:: " ++ prettyPrintType typ ++ ")" ++ resetColor 
prettyPrintStmt (Block stmts) =
    "{\n" ++ concatMap (\s -> "    " ++ prettyPrintStmt s ++ "\n") stmts ++ "}"
prettyPrintStmt (ReturnStmt expr) =
    "return " ++ prettyPrintExpression expr ++ ";"
prettyPrintStmt (WhileStmt cond stmt) =
    "while (" ++ prettyPrintExpression cond ++ ") " ++ prettyPrintStmt stmt
prettyPrintStmt (LocalVarDeclStmt typ name expr) =
    prettyPrintType typ ++ " " ++ name ++ maybeExpr expr ++ ";"
    where
        maybeExpr :: Maybe Expression -> String
        maybeExpr Nothing = ""
        maybeExpr (Just e) = " = " ++ prettyPrintExpression e
prettyPrintStmt (IfElseStmt cond stmt1 Nothing) =
    "if (" ++ prettyPrintExpression cond ++ ") " ++ prettyPrintStmt stmt1
prettyPrintStmt (IfElseStmt cond stmt1 (Just stmt2)) =
    "if (" ++ prettyPrintExpression cond ++ ") " ++
    prettyPrintStmt stmt1 ++ " else " ++ prettyPrintStmt stmt2
prettyPrintStmt (StmtExprStmt expr) =
    prettyPrintStmtExpr expr ++ ";"
prettyPrintStmt (Print str) =
    "System.out.println(" ++ str ++ ");"

-- Pretty Printer für Statement Expressions
prettyPrintStmtExpr :: StmtExpr -> String
prettyPrintStmtExpr (TypedStmtExpr stmtExpr typ) =
    prettyPrintStmtExpr stmtExpr
prettyPrintStmtExpr (AssignmentStmt expr1 expr2) =
    prettyPrintExpression expr1 ++ " = " ++ prettyPrintExpression expr2
prettyPrintStmtExpr (NewExpression newExpr) =
    prettyPrintNewExpr newExpr
prettyPrintStmtExpr (MethodCall (MethodCallExpr expr name args)) =
    prettyPrintExpression expr ++ "." ++ name ++ "(" ++
    commaSep (map prettyPrintExpression args) ++ ")"

-- Pretty Printer für Ausdrücke
prettyPrintExpression :: Expression -> String
prettyPrintExpression (TypedExpr expr typ) =
    " (" ++ prettyPrintExpression expr ++ dimColor ++ " :: " ++ prettyPrintType typ ++ resetColor ++ ")"
prettyPrintExpression ThisExpr = "this"
prettyPrintExpression SuperExpr = "super"
prettyPrintExpression (LocalOrFieldVarExpr name) = name
prettyPrintExpression (FieldVarExpr name) = "this." ++ name
prettyPrintExpression (LocalVarExpr name) = name
prettyPrintExpression (InstVarExpr expr name) =
    prettyPrintExpression expr ++ "." ++ name
prettyPrintExpression (UnaryOpExpr op expr) =
    prettyPrintUnaryOperator op ++ prettyPrintExpression expr
prettyPrintExpression (BinOpExpr expr1 op expr2) =
    prettyPrintExpression expr1 ++ " " ++
    prettyPrintBinaryOperator op ++ " " ++
    prettyPrintExpression expr2
prettyPrintExpression (IntLitExpr n) = show n
prettyPrintExpression (BoolLitExpr b) = if b then "true" else "false"
prettyPrintExpression (CharLitExpr c) = "'" ++ c ++ "'"
prettyPrintExpression (StringLitExpr s) = "\"" ++ s ++ "\""
prettyPrintExpression Null = "null"
prettyPrintExpression (StmtExprExpr stmtExpr) =
    prettyPrintStmtExpr stmtExpr

-- Pretty Printer für Neuausdrücke
prettyPrintNewExpr :: NewExpr -> String
prettyPrintNewExpr (NewExpr newType args) =
    "new " ++ prettyPrintNewType newType ++
    "(" ++ commaSep (map prettyPrintExpression args) ++ ")"

-- Hilfsfunktion zum Zusammenfügen von Strings mit Kommas
commaSep :: [String] -> String
commaSep = concat . intersperse ", "

-- Pretty Printer für Sichtbarkeit
prettyPrintVisibility :: Visibility -> String
prettyPrintVisibility Public = "public"

-- Pretty Printer für Typen
prettyPrintType :: Type -> String
prettyPrintType IntT = "int"
prettyPrintType BoolT = "boolean"
prettyPrintType CharT = "char"
prettyPrintType (NewTypeT newType) = prettyPrintNewType newType
prettyPrintType (FuncT argTypes retType) =
    prettyPrintType retType ++ " (" ++ commaSep (map prettyPrintType argTypes) ++ ")"
prettyPrintType VoidT = "void"

-- Pretty Printer für neue Typen
prettyPrintNewType :: NewType -> String
prettyPrintNewType (NewType name) = name

-- Pretty Printer für binäre Operatoren
prettyPrintBinaryOperator :: BinaryOperator -> String
prettyPrintBinaryOperator Plus = "+"
prettyPrintBinaryOperator Minus = "-"
prettyPrintBinaryOperator Times = "*"
prettyPrintBinaryOperator Divide = "/"
prettyPrintBinaryOperator And = "&&"
prettyPrintBinaryOperator Or = "||"
prettyPrintBinaryOperator Equal = "=="
prettyPrintBinaryOperator NotEqual = "!="
prettyPrintBinaryOperator Less = "<"
prettyPrintBinaryOperator Greater = ">"
prettyPrintBinaryOperator LessEq = "<="
prettyPrintBinaryOperator GreaterEq = ">="

-- Pretty Printer für unäre Operatoren
prettyPrintUnaryOperator :: UnaryOperator -> String
prettyPrintUnaryOperator UnaryMinus = "-"
prettyPrintUnaryOperator Not = "!"
prettyPrintUnaryOperator UnaryPlus = "+"