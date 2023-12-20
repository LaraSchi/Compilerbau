module Syntax where


-- Definition des Programmtyps
newtype Program = Program Class
             deriving (Show, Eq)

data Class = Class Newtype [FieldDecl] [MethodDecl]
           deriving (Show, Eq)

newtype Newtype = Newtype String
                deriving (Show, Eq)

data FieldDecl = FieldDecl Type String
               deriving (Show, Eq)

data MethodDecl = MethodDecl Visibility Type String [Parameter] [Stmt]
                deriving (Show, Eq)

data Parameter = Parameter Type String
               deriving (Show, Eq)

data Type = Int
          | Boolean
          | Char
          | NewtypeType Newtype
          deriving (Show, Eq)

data Visibility = Public
          deriving (Show, Eq)

data Stmt = ReturnStmt Expression
          | WhileStmt Expression [Stmt]
          | DeclarationStmt Type String (Maybe Expression)
          | IfStmt Expression [Stmt]
          | IfElseStmt Expression [Stmt] [Stmt] -- #TODO: alle [Stmt] zu BlockStmt
          | StmtExprStmt StmtExpr
          deriving (Show, Eq)

data StmtExpr = AssignmentStmt String Expression
              | NewExpression NewExpr
              | MethodCall MethodCallExpr
              deriving (Show, Eq)

data Expression = ThisExpr
                | SuperExpr
                | IntLitExpr Int
                | CharLitExpr String
                | BoolLitExpr Bool
                | StringLitExpr String
                | IdentifierExpr String
                | ParenExpr Expression
                | BinOpExpr Expression BinaryOperator Expression
                | UnaryOpExpr UnaryOperator Expression
                | StmtExprExpr StmtExpr
                deriving (Show, Eq)

data NewExpr = NewExpr ClassName [Expression]
             deriving (Show, Eq)

data MethodCallExpr = MethodCallExpr String [Expression]
                    deriving (Show, Eq)

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
                    deriving (Show, Eq)

data UnaryOperator = UnaryMinus
                   | Not
                   | UnaryPlus
                   deriving (Show, Eq)

newtype ClassName = ClassName String
                  deriving (Show, Eq)

