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
  class           { ClassT }
  public          { PublicT }
  intType         { IntTypeT }
  booleanType     { BooleanTypeT }
  charType        { CharTypeT }
  new             { NewT }
  return          { ReturnT }
  while           { WhileT }
  if              { IfT }
  else            { ElseT }
  this            { ThisT }
  super           { SuperT }
  true            { TrueT }
  false           { FalseT }
  '+'               { PlusT }
  '-'               { MinusT }
  '*'               { TimesT }
  '/'               { DivideT }
  '&&'              { AndT }
  '||'              { OrT }
  '=='              { EqualBoolT }
  '='               { EqualT }
  '!='              { NotEqualT }
  '<'               { LessT }
  '>'               { GreaterT }
  '<='              { LessEqT }
  '>='              { GreaterEqT }
  '!'               { NotT }
  ';'               { SemicolonT }
  ','               { CommaT }
  '.'               { DotT }
  '('               { LParT }
  ')'               { RParT }
  '{'               { LBraceT }
  '}'               { RBraceT }
  int               { IntT $$ }
  char              { CharT $$ }
  string            { StringT $$ }
  identifier        { IdentifierT $$ }

%%

Program
  : Class                 { Program $1 }

Class
  : class Newtype '{' FieldDecls MethodDecls '}'    { Class $2 $4 $5 }
  | class Newtype '{' FieldDecls '}'                { Class $2 $4 [] }
  | class Newtype '{' MethodDecls '}'               { Class $2 [] $4 }

Newtype
  : identifier            { Newtype $1 }

Type
  : intType                   { Int }
  | booleanType               { Boolean }
  | charType                  { Char }
  | Newtype                   { NewtypeType $1 }

FieldDecls
  : FieldDecl              { [$1] }
  | FieldDecl FieldDecls   { $1 : $2 }

MethodDecls
  : MethodDecl               { [$1] }
  | MethodDecl MethodDecls   { $1 : $2 } 

MethodDecl
  : public Type identifier '(' ParameterList ')' '{' BlockStmt '}'   { MethodDecl Public $2 $3 $5 $8 }
  | public Type identifier '(' ')' '{' BlockStmt '}'                 { MethodDecl Public $2 $3 [] $7 }

FieldDecl
  : Type identifier ';'              { FieldDecl $1 $2 }


ParameterList
  : Parameter                   { [$1] }
  | Parameter ',' ParameterList { $1 : $3 }

Parameter
  : Type identifier       { Parameter $1 $2 }

BlockStmt
  : Stmt                      { [$1] }
  | Stmt BlockStmt            { $1 : $2 }

Stmt
  : ReturnStmt            { $1 }
  | WhileStmt             { $1 }
  | DeclarationStmt       { $1 }
  | IfStmt                { $1 }
  | StmtExpr              { StmtExprStmt $1 }


StmtExpr
  : AssignmentStmt        { $1 }
  | NewExpression         { NewExpression $1 }
  | MethodCall            { MethodCall $1 }

Expression
  : this                            { ThisExpr }
  | super                           { SuperExpr }
  | int                             { IntLitExpr $1 }
  | char                            { CharLitExpr $1 }
  | BooleanLiteral                  { BoolLitExpr $1 }
  | string                          { StringLitExpr $1 }
  | identifier                      { IdentifierExpr $1 }
  | '(' Expression ')'              { ParenExpr $2 }
  | Expression BinaryOperator Expression { BinOpExpr $1 $2 $3 }
  | UnaryOperator Expression        { UnaryOpExpr $1 $2 }
  | StmtExpr                        { StmtExprExpr $1 }


NewExpression
  : new ClassName '(' ArgumentList ')' ';' { NewExpr $2 $4 }

MethodCall
  : identifier '(' ArgumentList ')' ';' { MethodCallExpr $1 $3 }

ArgumentList
  : Expression                      { [$1] }
  | Expression ',' ArgumentList     { $1 : $3 }

ReturnStmt
  : return Expression ';'     { ReturnStmt $2 }

WhileStmt
  : while '(' Expression ')' '{' BlockStmt '}' { WhileStmt $3 $6 }

DeclarationStmt
  : Type identifier ';'                 { DeclarationStmt $1 $2 Nothing }
  | Type identifier '=' Expression ';'  { DeclarationStmt $1 $2 (Just $4) }

IfStmt
  : if '(' Expression ')' '{' BlockStmt '}'                             { IfStmt $3 $6 }
  | if '(' Expression ')' '{' BlockStmt '}' else '{' BlockStmt '}'      { IfElseStmt $3 $6 $10 }

AssignmentStmt
  : identifier '=' Expression ';' { AssignmentStmt $1 $3 }

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
