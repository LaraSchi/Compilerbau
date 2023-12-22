module Syntax where


-- Definition des Programmtyps
newtype Program = Program Class
             deriving (Show, Eq, Read)

data Class = Class Newtype [FieldDecl] [MethodDecl]
           deriving (Show, Eq, Read)

newtype Newtype = Newtype String
                deriving (Show, Eq, Read)

data FieldDecl = FieldDecl Type String -- #TODO: manche trings zu Identifier
               deriving (Show, Eq, Read)

data MethodDecl = MethodDecl Visibility Type String [Parameter] BlockStmt
                deriving (Show, Eq, Read)

data Parameter = Parameter Type String
               deriving (Show, Eq, Read)

type BlockStmt =  [Stmt]

data Type = Int
          | Boolean
          | Char
          | NewtypeType Newtype
          | Func [Type] Type
          deriving (Show, Eq, Read)

data Visibility = Public
          deriving (Show, Eq, Read)

data Stmt = ReturnStmt Expression
          | WhileStmt Expression BlockStmt
          | DeclarationStmt Type String (Maybe Expression)
          | IfStmt Expression BlockStmt
          | IfElseStmt Expression BlockStmt BlockStmt -- #TODO: alle [Stmt] zu BlockStmt
          | StmtExprStmt StmtExpr
          deriving (Show, Eq, Read)

data StmtExpr = AssignmentStmt String Expression
              | NewExpression NewExpr
              | MethodCall MethodCallExpr
              deriving (Show, Eq, Read)

data Expression = ThisExpr
                | SuperExpr
                | IdentifierExpr String
                | InstVar Expression String
                | UnaryOpExpr UnaryOperator Expression
                | BinOpExpr Expression BinaryOperator Expression
                | IntLitExpr Int
                | BoolLitExpr Bool
                | CharLitExpr String
                | StringLitExpr String
                | Null
                | StmtExprExpr StmtExpr
                | ParenExpr Expression -- Expression in Klammern
                deriving (Show, Eq, Read)

data NewExpr = NewExpr ClassName [Expression]
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