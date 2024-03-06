module PrettyPrint where

import Syntax
import Data.List
import Debug.Trace

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
    prettyPrintClass cls {- ++ " (" ++ greenColor ++ show typed ++ resetColor ++ ")" -}

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
        maybeExpr (Just e) = " = " ++ prettyPrintExpression 0 e

-- Pretty Printer für Methodendeklarationen
prettyPrintMethodDecl :: MethodDecl -> String
prettyPrintMethodDecl (MethodDecl vis typ name params stmt) =
    "    " ++ prettyPrintVisibility vis ++ " " ++
    prettyPrintType typ ++ " " ++ name ++ "(" ++
    commaSep (map prettyPrintParameter params) ++ ") " ++
    prettyPrintStmt 2 stmt ++
    "\n"

-- Pretty Printer für Parameter
prettyPrintParameter :: Parameter -> String
prettyPrintParameter (Parameter typ name) =
    prettyPrintType typ ++ " " ++ name

-- Pretty Printer für Statements
prettyPrintStmt :: Int -> Stmt -> String
prettyPrintStmt c (TypedStmt stmt@(Block _) typ) =
     "(" ++prettyPrintStmt c stmt ++  "  :: " ++ greenColor ++ prettyPrintType typ ++ resetColor ++ ")"
prettyPrintStmt c (TypedStmt stmt typ) =
    prettyPrintStmt c stmt {- ++ greenColor ++ "  (:: " ++ prettyPrintType typ ++ ")" ++ resetColor  -}
prettyPrintStmt c (Block stmts) =
    "{\n" ++ concatMap (\s -> indent c ++ prettyPrintStmt (c + 1) s ++ "\n") stmts ++ indent (c -1) ++ "}"
prettyPrintStmt c (ReturnStmt expr) =
    "return " ++ prettyPrintExpression c expr ++ ";"
prettyPrintStmt c (WhileStmt cond stmt) =
    "while (" ++ prettyPrintExpression c cond ++ ") " ++ prettyPrintStmt c stmt
prettyPrintStmt c (LocalVarDeclStmt typ name expr) =
    prettyPrintType typ ++ " " ++ name ++ maybeExpr expr ++ ";"
    where
        maybeExpr :: Maybe Expression -> String
        maybeExpr Nothing = ""
        maybeExpr (Just e) = " = " ++ prettyPrintExpression c e
prettyPrintStmt c (IfElseStmt cond stmt1 Nothing) =
    "if (" ++ prettyPrintExpression c cond ++ ") " ++ prettyPrintStmt c stmt1
prettyPrintStmt c (IfElseStmt cond stmt1 (Just stmt2)) =
    "if (" ++ prettyPrintExpression c cond ++ ") " ++
    prettyPrintStmt (c+1) stmt1 ++ " else " ++ prettyPrintStmt (c+1) stmt2
prettyPrintStmt c (StmtExprStmt expr) =
    prettyPrintStmtExpr c expr ++ ";"
prettyPrintStmt c (Print str) =
    "System.out.println(" ++ str ++ ");"

-- Pretty Printer für Statement Expressions
prettyPrintStmtExpr :: Int -> StmtExpr -> String
prettyPrintStmtExpr c (TypedStmtExpr stmtExpr typ) =
    prettyPrintStmtExpr c stmtExpr
prettyPrintStmtExpr c (AssignmentStmt expr1 expr2) =
    prettyPrintExpression c expr1 ++ " = " ++ prettyPrintExpression c expr2
prettyPrintStmtExpr c (NewExpression newExpr) =
    prettyPrintNewExpr newExpr
prettyPrintStmtExpr c (MethodCall (MethodCallExpr Null name args)) =
    name ++ "(" ++ commaSep (map (prettyPrintExpression c) args) ++ ")"
prettyPrintStmtExpr c (MethodCall (MethodCallExpr expr name args)) =
    prettyPrintExpression c expr ++ "." ++ name ++ "(" ++ commaSep (map (prettyPrintExpression c) args) ++ ")"

-- Pretty Printer für Ausdrücke
prettyPrintExpression :: Int -> Expression -> String
prettyPrintExpression c (TypedExpr expr typ) =
    dimColor ++ "(" ++ resetColor ++ prettyPrintExpression c expr ++ dimColor ++ " :: " ++ prettyPrintType typ ++ ")" ++ resetColor
prettyPrintExpression c ThisExpr = "this"
prettyPrintExpression c SuperExpr = "super"
prettyPrintExpression c (LocalOrFieldVarExpr name) = name
prettyPrintExpression c (FieldVarExpr name) = "this." ++ name
prettyPrintExpression c (LocalVarExpr name) = name
prettyPrintExpression c (InstVarExpr Null name) = name
prettyPrintExpression c (InstVarExpr expr name) =
    prettyPrintExpression c expr ++ "." ++ name
prettyPrintExpression c (UnaryOpExpr op expr) =
    prettyPrintUnaryOperator op ++ prettyPrintExpression c expr
prettyPrintExpression c (BinOpExpr expr1 op expr2) =
    prettyPrintExpression c expr1 ++ " " ++
    prettyPrintBinaryOperator op ++ " " ++
    prettyPrintExpression c expr2
prettyPrintExpression c (IntLitExpr n) = show n
prettyPrintExpression c (BoolLitExpr b) = if b then "true" else "false"
prettyPrintExpression c (CharLitExpr ch) = "'" ++ ch ++ "'"
prettyPrintExpression c (StringLitExpr s) = "\"" ++ s ++ "\""
prettyPrintExpression c Null = "this"
prettyPrintExpression c (StmtExprExpr stmtExpr) =
    prettyPrintStmtExpr c stmtExpr

-- Pretty Printer für Neuausdrücke
prettyPrintNewExpr :: NewExpr -> String
prettyPrintNewExpr (NewExpr newType args) =
    "new " ++ prettyPrintNewType newType ++
    "(" ++ commaSep (map (prettyPrintExpression 0) args) ++ ")"

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
prettyPrintNewType (NewType "xIntT")        = resetColor ++ redColor ++ "int" ++ resetColor
prettyPrintNewType (NewType "xBoolT")       = resetColor ++ redColor ++ "boolean" ++ resetColor
prettyPrintNewType (NewType "xCharT")       = resetColor ++ redColor ++ "char" ++ resetColor
prettyPrintNewType (NewType "xVoidT")       = resetColor ++ redColor ++ "void" ++ resetColor
prettyPrintNewType all@(NewType ('x':name)) = resetColor ++ redColor ++ name ++ resetColor
prettyPrintNewType (NewType name)           = name

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


indent :: Int -> String
indent c = concat $ replicate c "    "