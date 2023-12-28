module Syntax where

type Typed = Bool

-- Definition des Programmtyps
data Program = Program Class Typed
             deriving (Show, Eq, Read)

data Class = Class NewType [FieldDecl] [MethodDecl]
           deriving (Show, Eq, Read)

newtype NewType = NewType String
                deriving (Show, Eq, Read)

data FieldDecl = FieldDecl Type String -- #TODO: manche trings zu Identifier
               deriving (Show, Eq, Read)

data MethodDecl = MethodDecl Visibility Type String [Parameter] BlockStmt
                deriving (Show, Eq, Read)

data Parameter = Parameter Type String
               deriving (Show, Eq, Read)

type BlockStmt =  [Stmt]

data Type = IntT
          | BoolT
          | CharT
          | StringT -- #TODO: zu Parser hinzufügen
          | NewTypeT NewType
          | FuncT [Type] Type
          | VoidT
          deriving (Show, Eq, Read)

data Visibility = Public
          deriving (Show, Eq, Read)

data Stmt = TypedStmt Stmt Type
          | ReturnStmt Expression
          | WhileStmt Expression BlockStmt
          | LocalVarDeclStmt Type String
          | IfStmt Expression BlockStmt
          | IfElseStmt Expression BlockStmt (Maybe BlockStmt)
          | StmtExprStmt StmtExpr
          deriving (Show, Eq, Read)

data StmtExpr = TypedStmtExpr StmtExpr Type
              | AssignmentStmt Expression Expression
              | NewExpression NewExpr
              | MethodCall MethodCallExpr
              deriving (Show, Eq, Read)

data Expression = TypedExpr Expression Type 
                | ThisExpr
                | SuperExpr
                | IdentifierExpr String --LocalOrFieldVar
                | InstVar Expression String
                | UnaryOpExpr UnaryOperator Expression
                | BinOpExpr Expression BinaryOperator Expression
                | IntLitExpr Int
                | BoolLitExpr Bool
                | CharLitExpr String
                | StringLitExpr String
                | Null
                | StmtExprExpr StmtExpr
                deriving (Show, Eq, Read)

data NewExpr = NewExpr NewType [Expression]
             deriving (Show, Eq, Read)

data MethodCallExpr = MethodCallExpr Expression String [Expression]
                    deriving (Show, Eq, Read)


data BinaryOperator = Plus
                    | Minus
                    | Times
                    | Divide
                    | And
                    | Or
                    | Equal
                    | NotEqual
                    | Less
                    | Greater
                    | LessEq
                    | GreaterEq
                    deriving (Show, Eq, Read)

data UnaryOperator = UnaryMinus
                   | Not
                   | UnaryPlus
                   deriving (Show, Eq, Read)

newtype ClassName = ClassName String
                  deriving (Show, Eq, Read)

-- TODO: evtl umbenennen? Zb Expression -> Expr?