module Semantics
    ( checkSemantics
    ) where

import Syntax
import Control.Monad.Except
import Control.Monad.State (gets, modify, State, MonadState (put, get), runState)


-- data LocalOrField = FieldVar | LocalVar

data TypeState = TypeState { name             :: String,
                             localTypeset     :: [(String, Type)],
                             fieldTypeset     :: [(String, Type)],
                             errors           :: [String],
                             currentType      :: Type }

type TypeStateM = State TypeState

checkSemantics :: Program -> Either String Program
checkSemantics p = case runTypeStateM p of
    (p',[]) -> Right p
    (p',es) -> Left $ head es -- #TODO: errors schön zu errormessage konkatenieren 


runTypeStateM :: Program -> (Program, [String])
runTypeStateM p = fmap errors . runState (checkSProg p) $ TypeState [] [] [] [] NoT

checkSProg :: Program -> TypeStateM Program
checkSProg p@(Program (Class (Newtype n) fd md) _) = do
        modify $ \s -> s {name = n}
        fillTypeSetFields fd
        fillTypesSetMethod md
        --mdTyped <- checkMethods md
        let mdTyped = md
        return (Program (Class (Newtype n) fd mdTyped) True)

fillTypeSetFields :: [FieldDecl] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {fieldTypeset = types}) 
    where types = map (\(FieldDecl t s) -> (s,t)) fds

fillTypesSetMethod :: [MethodDecl] -> TypeStateM ()
fillTypesSetMethod mds = do
    ts <- gets localTypeset
    let funcTypes = map buildFuncType mds
    modify (\s -> s {localTypeset = ts ++ funcTypes})

buildFuncType :: MethodDecl -> (String, Type)
buildFuncType (MethodDecl _ t n ps _) = (n, FuncT param t)
    where param = map (\(Parameter ty st) -> ty) ps

checkMethods :: [MethodDecl] -> TypeStateM [MethodDecl]
checkMethods = mapM checkMethod

checkMethod :: MethodDecl -> TypeStateM MethodDecl
checkMethod (MethodDecl v t s ps stmts) = do
    typedStmts <- mapM checkStmt stmts
    return $ MethodDecl v t s ps typedStmts


checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (ReturnStmt e)                    = checkExpression e >>= \eTyped -> return $ ReturnStmt eTyped
checkStmt (WhileStmt e stmts)               = do
    eTyped     <- checkExpression e 
    stmtsTyped <- mapM checkStmt stmts
    return (WhileStmt eTyped stmtsTyped)
checkStmt (LocalOrFieldVarDeclStmt t s)    = undefined
checkStmt (IfElseStmt e bs Nothing)         = do
    eTyped     <- checkExpression e 
    stmtsTyped <- mapM checkStmt bs
    return (IfElseStmt eTyped stmtsTyped Nothing) 
checkStmt (IfElseStmt e bs1 (Just bs2))      = do
    eTyped      <- checkExpression e 
    bsTyped1 <- mapM checkStmt bs1
    bsTyped2 <- mapM checkStmt bs2
    return (IfElseStmt eTyped bsTyped1 (Just bsTyped2))
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seTyped -> return $ StmtExprStmt seTyped

-- TODO: alle Expr typen 
-- TODO: evtl. zu haskell typen? 
checkExpression :: Expression -> TypeStateM Expression
checkExpression ThisExpr             = undefined
checkExpression SuperExpr            = undefined
checkExpression (IdentifierExpr s)   = undefined
checkExpression (InstVar e s)        = undefined
checkExpression (UnaryOpExpr op e)   = checkUnary op e >>= \eTyped -> return $ UnaryOpExpr op eTyped
checkExpression (BinOpExpr e1 op e2) = checkBinary e1 op e2
checkExpression e@(IntLitExpr i)     = return $ TypedExpr e IntT
checkExpression e@(BoolLitExpr b)    = return $ TypedExpr e BoolT
checkExpression e@(CharLitExpr [c])  = return $ TypedExpr e CharT
checkExpression e@(StringLitExpr s)  = return $ TypedExpr e StringT
checkExpression Null                 = return $ TypedExpr Null NoT
checkExpression (StmtExprExpr se)    = checkStmtExpr se >>= \seTyped -> return $ StmtExprExpr seTyped
checkExpression _                    = undefined

-- #TODO: überall den eigentlichen check hinzufügen :D
checkUnary :: UnaryOperator -> Expression -> TypeStateM Expression
checkUnary op e         = checkExpression e
checkUnary UnaryMinus e = checkExpression e
checkUnary UnaryPlus e  = checkExpression e

checkBinary :: Expression -> BinaryOperator -> Expression -> TypeStateM Expression
checkBinary e1 op e2    = checkExpression e1 >>= \e1Typed -> checkExpression e2 >>= \e2Typed -> checkBinary e1Typed op e2Typed

checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr (AssignmentStmt s e) = undefined
checkStmtExpr (NewExpression n)    = undefined
checkStmtExpr (MethodCall m)       = undefined 
-- TODO: #error "should not appear"


-- #TODO: 
{- 
    - rekursive typisierungsfunktion
    - alle Methoden/Variabeln klar deklariert?
    - erster Plan: State Monad beim rekursiven Typchecken, alle 
    Variabeln und Methoden zwischenspeichern zum typchecken?
    - StateMonad (Menge Typnamen O = {id:ty})

-}

