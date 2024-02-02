{
module Lexer (alexScanTokens, Token(..)) where
}

%wrapper "basic"

$digit  = 0-9
$char   = [a-zA-Z]
$string = [a-zA-Z_0-9]

tokens :-
  $white+                     ;
  "//".*;
  class                     { \_ -> ClassT }
  public                    { \_ -> PublicT }
  int                       { \_ -> IntTypeT }
  boolean                   { \_ -> BooleanTypeT }
  char                      { \_ -> CharTypeT }
  new                       { \_ -> NewT }
  return                    { \_ -> ReturnT }
  while                     { \_ -> WhileT }
  if                        { \_ -> IfT }
  else                      { \_ -> ElseT }
  this                      { \_ -> ThisT }
  super                     { \_ -> SuperT }
  true                      { \_ -> TrueT }
  false                     { \_ -> FalseT }
  null                      { \_ -> NullT }
  println                   { \_ -> PrintlnT }
  "+"                         { \_ -> PlusT }
  "-"                         { \_ -> MinusT }
  "*"                         { \_ -> TimesT }
  "/"                         { \_ -> DivideT }
  "&&"                        { \_ -> AndT }
  "||"                        { \_ -> OrT }
  "=="                        { \_ -> EqualBoolT }
  "!="                        { \_ -> NotEqualT }
  "<"                         { \_ -> LessT }
  ">"                         { \_ -> GreaterT }
  "="                         { \_ -> EqualT }
  "<="                        { \_ -> LessEqT }
  ">="                        { \_ -> GreaterEqT }
  "!"                         { \_ -> NotT }
  ";"                         { \_ -> SemicolonT }
  ","                         { \_ -> CommaT }
  "."                         { \_ -> DotT }
  "("                         { \_ -> LParT }
  ")"                         { \_ -> RParT }
  "{"                         { \_ -> LBraceT }
  "}"                         { \_ -> RBraceT }
  $digit+                     { \s -> IntTo (read s) }
  "'"$char"'"                 { \s -> CharTo s }
  "~"$string+"~"              { \s -> StringTo s }
  $string+                    { \s -> IdentifierT s }


{


data Token = ClassT
           | PublicT
           | IntTypeT
           | BooleanTypeT
           | CharTypeT
           | NewT
           | ReturnT
           | WhileT
           | IfT
           | ElseT
           | ThisT
           | SuperT
           | TrueT
           | FalseT
           | NullT
           | PlusT
           | MinusT
           | TimesT
           | DivideT
           | AndT
           | OrT
           | EqualT
           | EqualBoolT
           | NotEqualT
           | LessT
           | GreaterT
           | LessEqT
           | GreaterEqT
           | NotT
           | PrintlnT
           | SemicolonT
           | CommaT
           | DotT
           | LParT
           | RParT
           | LBraceT
           | RBraceT
           | IntTo Int
           | CharTo String
           | StringTo String
           | IdentifierT String
    deriving (Show, Eq)
}