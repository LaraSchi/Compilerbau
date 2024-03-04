{
module Parser where

import Lexer
import qualified Data.ByteString.Char8 as BS
import Syntax

import Control.Monad.Except
}

%name calc
%monad { Except String } { (>>=) } { return }
%tokentype { Token }
%error { parseError }

%token
  class                     { ClassT }
  public                    { PublicT }
  intType                   { IntTypeT }
  booleanType               { BooleanTypeT }
  charType                  { CharTypeT }
  voidType                  { VoidTypeT }
  new                       { NewT }
  return                    { ReturnT }
  while                     { WhileT }
  if                        { IfT }
  else                      { ElseT }
  this                      { ThisT }
  super                     { SuperT }
  true                      { TrueT }
  false                     { FalseT }
  null                      { NullT }
  println                   { PrintlnT }
  '+'                       { PlusT }
  '-'                       { MinusT }
  '*'                       { TimesT }
  '/'                       { DivideT }
  '&&'                      { AndT }
  '||'                      { OrT }
  '=='                      { EqualBoolT }
  '='                       { EqualT }
  '!='                      { NotEqualT }
  '<'                       { LessT }
  '>'                       { GreaterT }
  '<='                      { LessEqT }
  '>='                      { GreaterEqT }
  '!'                       { NotT }
  ';'                       { SemicolonT }
  ','                       { CommaT }
  '.'                       { DotT }
  '('                       { LParT }
  ')'                       { RParT }
  '{'                       { LBraceT }
  '}'                       { RBraceT }
  int                       { IntTo $$ }
  char                      { CharTo $$ }
  string                    { StringTo $$ }
  identifier                { IdentifierT $$ }

%%

Program
  : Class                 { Program $1 False }

Class
  : class Newtype '{' Fields MethodDecls '}'    { Class $2 $4 $5 }
  | class Newtype '{' Fields '}'                { Class $2 $4 [] }
  | class Newtype '{' MethodDecls '}'           { Class $2 [] $4 }
  | class Newtype '{' '}'                       { Class $2 [] [] }

Newtype
  : identifier            { NewType $1 }

Type
  : intType                   { IntT }
  | booleanType               { BoolT }
  | charType                  { CharT }
  | voidType                  { VoidT }
  | Newtype                   { NewTypeT $1 }

Fields
  : Field             { $1 }
  | Field Fields   { $1 ++ $2 }

MethodDecls
  : MethodDecl               { [$1] }
  | MethodDecl MethodDecls   { $1 : $2 } 

MethodDecl
  : public Type identifier '(' ParameterList ')' '{' BlockStmt '}'   { MethodDecl Public $2 $3 $5 $8 }
  | public Type identifier '(' ')' '{' BlockStmt '}'                 { MethodDecl Public $2 $3 [] $7 }
  | public      identifier '(' ParameterList ')' '{' BlockStmt '}'   { MethodDecl Public VoidT $2 $4 $7 }
  | public      identifier '(' ')' '{' BlockStmt '}'                 { MethodDecl Public VoidT $2 [] $6 }

Field
  : Type identifier ';'                  { [FieldDecl $1 $2 Nothing] }
  | Type identifier '=' Expression ';'   { [FieldDecl $1 $2 (Just $4)] } -- > TODO: Liste aus FieldRef und FieldDecl


ParameterList
  : Parameter                   { [$1] }
  | Parameter ',' ParameterList { $1 : $3 }

Parameter
  : Type identifier       { Parameter $1 $2 }

BlockStmt : BlockStmtList     { Block $1 }

BlockStmtList
  : Stmt                      { [$1] }
  | Stmt BlockStmtList            { $1 : $2 }

Stmt
  : ReturnStmt ';'           { $1 }
  | WhileStmt             { $1 }
  | DeclarationStmt  ';'     { $1 }
  | IfStmt                { $1 }
  | println '(' string ')' ';'   { Print $3 }
  | StmtExpr ';'              { StmtExprStmt $1 }


StmtExpr
  : AssignmentStmt        { $1 }
  | NewExpression         { NewExpression $1 }
  | MethodCall            { MethodCall $1 }

Expression
  : this                            { ThisExpr }
  | super                           { SuperExpr }
  | identifier                      { LocalOrFieldVarExpr $1 }
  | StmtExpr                        { StmtExprExpr $1 }
  | this '.' identifier             { FieldVarExpr $3 }
  | Expression '.' identifier       { InstVarExpr $1 $3 }
  | UnaryOperator Expression        { UnaryOpExpr $1 $2 }
  | Expression BinaryOperator Expression { BinOpExpr $1 $2 $3 }
  | int                             { IntLitExpr $1 }
  | BooleanLiteral                  { BoolLitExpr $1 }
  | char                            { CharLitExpr $1 }
  | string                          { StringLitExpr $1 }
  | null                            { Null }
  | '(' Expression ')'              { $2 }


NewExpression
  : new Newtype '(' ArgumentList ')' { NewExpr $2 $4 }
  | new Newtype '(' ')'              { NewExpr $2 [] }

MethodCall
  : Expression '.' identifier '(' ArgumentList ')' { MethodCallExpr $1 $3 $5 }
  | Expression '.' identifier '(' ')'              { MethodCallExpr $1 $3 [] }
  | this '.' identifier '(' ArgumentList ')'       { MethodCallExpr Null $3 $5 }
  | this '.' identifier '(' ')'                    { MethodCallExpr Null $3 [] }
  | identifier '(' ArgumentList ')'                { MethodCallExpr Null $1 $3 }
  | identifier '('  ')'                            { MethodCallExpr Null $1 [] }

ArgumentList
  : Expression                      { [$1] }
  | Expression ',' ArgumentList     { $1 : $3 }

ReturnStmt
  : return Expression     { ReturnStmt $2 }

WhileStmt
  : while '(' Expression ')' '{' BlockStmt '}' { WhileStmt $3 $6 }

DeclarationStmt
  : Type identifier                 { LocalVarDeclStmt $1 $2 Nothing}
  | Type identifier '=' Expression  { LocalVarDeclStmt $1 $2 (Just $4) }

IfStmt
  : if '(' Expression ')' '{' BlockStmt '}'                             { IfElseStmt $3 $6 Nothing }
  | if '(' Expression ')' '{' BlockStmt '}' else '{' BlockStmt '}'      { IfElseStmt $3 $6 (Just $10) }

AssignmentStmt
  : Expression '=' Expression                   { AssignmentStmt $1 $3 }

BinaryOperator
  : '+'       { Plus }
  | '-'       { Minus }
  | '*'       { Times }
  | '/'       { Divide }
  | '&&'      { And }
  | '||'      { Or }
  | '=='      { Equal }
  | '!='      { NotEqual }
  | '<'       { Less }
  | '>'       { Greater }
  | '<='      { LessEq }
  | '>='      { GreaterEq }

UnaryOperator
  : '-'       { UnaryMinus }
  | '!'       { Not }
  | '+'       { UnaryPlus } -- Unary Plus

BooleanLiteral
  : true      { True }
  | false     { False }

ClassName
  : identifier { ClassName $1 }



{
parseError :: [Token] -> a
parseError e = error $ show e

parse = runExcept . calc . alexScanTokens

}
