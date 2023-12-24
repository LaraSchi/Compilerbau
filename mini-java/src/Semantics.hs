module Semantics ( checkSemantics ) where

import Syntax
import Control.Monad.State (gets, modify, State, runState)

-- import Debug.Trace --Debugging, TODO: remove


data TypeState = TypeState { name             :: String,
                             localTypeset     :: [(String, Type)],
                             fieldTypeset     :: [(String, Type)],
                             errors           :: [String]}

type TypeStateM = State TypeState

checkSemantics :: Program -> Either String Program
checkSemantics p = case runTypeStateM p of
    (p',[]) -> Right p'
    (_,es) -> Left $ head es -- #TODO: errors schön zu errormessage konkatenieren 


runTypeStateM :: Program -> (Program, [String])
runTypeStateM p = fmap errors . runState (checkSProg p) $ TypeState [] [] [] []

checkSProg :: Program -> TypeStateM Program
checkSProg (Program (Class (Newtype n) fd md) _) = do
        modify $ \s -> s {name = n}
        fillTypeSetFields fd
        fillTypesSetMethod md
        mdTyped <- mapM checkMethod md
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
    where param = map (\(Parameter ty _) -> ty) ps


checkMethod :: MethodDecl -> TypeStateM MethodDecl
checkMethod (MethodDecl v t s ps stmts) = do
    typedStmts <- mapM checkStmt stmts
    return $ MethodDecl v t s ps typedStmts


checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (ReturnStmt e)                    = checkExpression e >>= \eTyped -> return $ TypedStmt (ReturnStmt eTyped) (getTypeE eTyped)
checkStmt (WhileStmt e stmts)               = do
    eTyped     <- checkBoolExpr e 
    stmtsTyped <- mapM checkStmt stmts
    return $ TypedStmt (WhileStmt eTyped stmtsTyped) VoidT
checkStmt v@(LocalVarDeclStmt t s)          = do
    locals <- gets fieldTypeset
    modify (\state -> state {fieldTypeset = (s,t):locals}) 
    return $ TypedStmt v t
checkStmt (IfElseStmt e bs Nothing)         = do
    eTyped     <- checkBoolExpr e 
    stmtsTyped <- mapM checkStmt bs
    return $ TypedStmt (IfElseStmt eTyped stmtsTyped Nothing) VoidT
checkStmt (IfElseStmt e bs1 (Just bs2))      = do
    eTyped   <- checkBoolExpr e 
    bsTyped1 <- mapM checkStmt bs1
    bsTyped2 <- mapM checkStmt bs2
    return $ TypedStmt (IfElseStmt eTyped bsTyped1 (Just bsTyped2)) VoidT
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seTyped -> return $ TypedStmt (StmtExprStmt seTyped) VoidT
checkStmt _                                 = error "checkStmt called on already typed Expression"

checkBoolExpr :: Expression -> TypeStateM Expression
checkBoolExpr e = checkExpression e >>= \typed -> case getTypeE typed of
    BoolT -> return typed
    _    -> error "Bool hier sollte eine passende Error message erstellt werden"


checkIntExpr :: Expression -> TypeStateM Expression
checkIntExpr e = checkExpression e >>= \typed -> case getTypeE typed of
    IntT -> return typed
    _    -> error "Int hier sollte eine passende Error message erstellt werden"


-- TODO: alle Expr typen 
-- TODO: evtl. zu haskell typen? 
checkExpression :: Expression -> TypeStateM Expression
checkExpression ThisExpr             = return $ TypedExpr ThisExpr VoidT -- #TODO: ändern zu ObjektTypen
checkExpression SuperExpr            = return $ TypedExpr SuperExpr VoidT -- #TODO: ändern zu ?
checkExpression i@(IdentifierExpr _) = return $ TypedExpr i IntT -- #TOOD : checken, was für eine Art von Var und Typ zurückgeben
checkExpression v@(InstVar _ _)      = return $ TypedExpr v StringT
checkExpression (UnaryOpExpr op e)   = checkUnary op e
checkExpression (BinOpExpr e1 op e2) = checkBinary e1 op e2
checkExpression e@(IntLitExpr _)     = return $ TypedExpr e IntT
checkExpression e@(BoolLitExpr _)    = return $ TypedExpr e BoolT
checkExpression e@(CharLitExpr [_])  = return $ TypedExpr e CharT -- evtl nochmal iwo prüfen, das char
checkExpression e@(StringLitExpr _)  = return $ TypedExpr e StringT
checkExpression Null                 = return $ TypedExpr Null VoidT
checkExpression (StmtExprExpr se)    = checkStmtExpr se >>= \seTyped -> return $ TypedExpr (StmtExprExpr seTyped) (getTypeSE seTyped)
checkExpression _                    = error "checkExpression called on already typed Expression"
 

-- #TODO: Hilfsfunktionen
checkUnary :: UnaryOperator -> Expression -> TypeStateM Expression
checkUnary Not e        = checkBoolExpr e >>= \typedE -> return $ TypedExpr (UnaryOpExpr Not typedE) BoolT
checkUnary op e         = checkIntExpr e  >>= \typedE -> return $ TypedExpr (UnaryOpExpr op typedE) IntT


checkBinary :: Expression -> BinaryOperator -> Expression -> TypeStateM Expression
checkBinary e1 op e2  = case op of
    And         -> checkBoolBinary
    Or          -> checkBoolBinary
    Equal       -> checkCompBinary
    NotEqual    -> checkCompBinary
    Less        -> checkIntBinary BoolT
    Greater     -> checkIntBinary BoolT
    LessEq      -> checkIntBinary BoolT
    GreaterEq   -> checkIntBinary BoolT
    Plus        -> checkIntBinary IntT
    Minus       -> checkIntBinary IntT
    Times       -> checkIntBinary IntT
    Divide      -> checkIntBinary IntT
    where checkIntBinary t = checkIntExpr e1 >>= \typed1 -> checkIntExpr e2 >>= \typed2 -> return $ TypedExpr (BinOpExpr typed1 op typed2) t
          checkBoolBinary  = checkBoolExpr e1 >>= \typed1 -> checkBoolExpr e2 >>= \typed2 -> return $ TypedExpr (BinOpExpr typed1 op typed2) BoolT
          checkCompBinary  = checkSameExpr e1 e2 >>= \(typed1, typed2) -> return $ TypedExpr (BinOpExpr typed1 op typed2) BoolT

checkSameExpr :: Expression -> Expression -> TypeStateM (Expression, Expression)
checkSameExpr e1 e2 = checkExpression e1 >>= \typed1 -> checkExpression e2 >>= \typed2 -> if getTypeE typed1 == getTypeE typed2 then return (e1,e2) else error "hier sollte eine passende Error message erstellt werden"



-- type checking statements & wraping StmtExpr into TypedStmtExpr
checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr (AssignmentStmt s e) = checkExpression e >>= \eTyped -> return $ TypedStmtExpr (AssignmentStmt s eTyped) VoidT
checkStmtExpr (NewExpression n)    = checkNew n >>= \nTyped -> return $ TypedStmtExpr (NewExpression nTyped) VoidT -- #TODO: Prüfen, ob Typ überhaupt richtig
checkStmtExpr (MethodCall m)       = checkMethodCall m >>= \mTyped -> return $ TypedStmtExpr (MethodCall mTyped) VoidT -- #TODO: Anzahl und Art Parameter checken
checkStmtExpr _                    = error "checkStmtExpr called on already typed Stmt"


getTypeE :: Expression -> Type
getTypeE (TypedExpr _ t) = t
getTypeE _               = error "blub"

getTypeSE :: StmtExpr -> Type
getTypeSE (TypedStmtExpr _ t) = t
getTypeSE _                    = error "blub"

checkNew :: NewExpr -> TypeStateM NewExpr
checkNew (NewExpr cn es) = do
    -- #TODO check Type of ClassName
    eTyped      <- mapM checkExpression es
    return $ NewExpr cn eTyped


checkMethodCall :: MethodCallExpr -> TypeStateM MethodCallExpr
checkMethodCall (MethodCallExpr e s es) = do
    eTyped  <- checkExpression e
    esTyped <- mapM checkExpression es
    return $ MethodCallExpr eTyped s esTyped

-- #TODO: 
{- 
    TODOs:
    - alles aufräumen/umschreiben/ Hilfsfunktionen/Kommentare
    - TypeX hinzufügen
    - checkExpression/Stmt/StmtExpr -> an dieser Stelle immer zu TypedX umwandeln. dazwischen immer typeX hochreichen
    - dadrüber immer prüfen, ob jeweiliger Typ an der Stelle passend.
    - sinnvolle Error messages einbauen:
        - an Typcheckstellen, zB: "Typfehler: If-Statement: Statement should be Typ Bool, but is Int", etc."

-}

