module Syntax where


-- Definition des Programmtyps
newtype Program = Program Class
             deriving (Show, Eq, Read)

data Class = Class Newtype [FieldDecl] [MethodDecl]
           deriving (Show, Eq, Read)

newtype Newtype = Newtype String
                deriving (Show, Eq, Read)

data FieldDecl = FieldDecl Type String
               deriving (Show, Eq, Read)

data MethodDecl = MethodDecl Visibility Type String [Parameter] [Stmt]
                deriving (Show, Eq, Read)

data Parameter = Parameter Type String
               deriving (Show, Eq, Read)

data Type = Int
          | Boolean
          | Char
          | NewtypeType Newtype
          deriving (Show, Eq, Read)

data Visibility = Public
          deriving (Show, Eq, Read)

data Stmt = ReturnStmt Expression
          | WhileStmt Expression [Stmt]
          | DeclarationStmt Type String (Maybe Expression)
          | IfStmt Expression [Stmt]
          | IfElseStmt Expression [Stmt] [Stmt] -- #TODO: alle [Stmt] zu BlockStmt
          | StmtExprStmt StmtExpr
          deriving (Show, Eq, Read)

data StmtExpr = AssignmentStmt String Expression
              | NewExpression NewExpr
              | MethodCall MethodCallExpr
              deriving (Show, Eq, Read)

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
                deriving (Show, Eq, Read)

data NewExpr = NewExpr ClassName [Expression]
             deriving (Show, Eq, Read)

data MethodCallExpr = MethodCallExpr String [Expression]
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

