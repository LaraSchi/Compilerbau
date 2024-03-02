module Syntax where

type Typed = Bool

-- Definition des Programmtyps
data Program = Program Class Typed
             deriving (Show, Eq, Read)

data Class = Class NewType [Field] [MethodDecl]
           deriving (Show, Eq, Read)

newtype NewType = NewType String
                deriving (Show, Eq, Read)

data Field = FieldDecl Type String (Maybe Expression)
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
          | LocalVarDecStmt Type String (Maybe Expression)
          | VarRefParse String Expression -- noch nicht sicher, ob Field oder Local
          | LocalVarRef String Expression -- LocalRef
          | FieldVarRef String Expression -- FieldRef (zugeordnet oder durch this.)
          | IfStmt Expression BlockStmt
          | IfElseStmt Expression BlockStmt (Maybe BlockStmt)
          | StmtExprStmt StmtExpr
          | Print String
          -- Vllt hier Field Ref zum Verändern von fieldVars? 
          deriving (Show, Eq, Read)

data StmtExpr = TypedStmtExpr StmtExpr Type
              | AssignmentStmt Expression Expression -- #TODO: first Expr -> String
              | NewExpression NewExpr
              | MethodCall MethodCallExpr
              deriving (Show, Eq, Read)

data Expression = TypedExpr Expression Type 
                | ThisExpr
                | SuperExpr
                | IdentifierExpr String --LocalOrFieldVar
                | InstVar Expression String  -- ## FieldRef im CP TODO: Maybe This vllt?
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

data MethodCallExpr = MethodCallExpr Expression String [Expression] -- Todo Wie in den Folien umschreiebn
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