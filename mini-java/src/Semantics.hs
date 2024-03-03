module Semantics ( checkSemantics ) where

import Syntax
import Control.Monad.State (gets, get, modify, State, runState)

import Debug.Trace --Debugging, TODO: remove


data TypeState = TypeState { classType        :: Type,
                             localTypeset     :: [(String, Type)],
                             fieldTypeset     :: [(String, Type)],
                             errors           :: [String] }
                             deriving Show 

type TypeStateM = State TypeState

checkSemantics :: Program -> Either String Program
checkSemantics p = case runTypeStateM p of
    (p',[]) -> Right p'
    (_,es) -> Left $ head es -- #TODO: errors schön zu errormessage konkatenieren 


runTypeStateM :: Program -> (Program, [String])
runTypeStateM p = fmap errors . runState (checkSProg p) $ TypeState VoidT [] [] []

checkSProg :: Program -> TypeStateM Program
checkSProg (Program (Class n fd md) _) = do
        modify $ \s -> s {classType = NewTypeT n}
        fillTypeSetFields fd
        fillTypesSetMethod md
        mdTyped <- mapM checkMethod md
        return (Program (Class n fd mdTyped) True)

fillTypeSetFields :: [Field] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {fieldTypeset = typesOf fds}) 
    where types = typesOf fds

typesOf :: [Field] -> [(String, Type)]
typesOf []                 = []
typesOf (FieldDecl t s me:ys) = (s,t):typesOf ys
typesOf (_:ys)               = typesOf ys

fillTypesSetMethod :: [MethodDecl] -> TypeStateM ()
fillTypesSetMethod mds = do
    ts <- gets localTypeset
    let funcTypes = concatMap buildFuncType mds
    modify (\s -> s {localTypeset = ts ++ funcTypes})

buildFuncType :: MethodDecl -> [(String, Type)]
buildFuncType (MethodDecl _ t n ps _) = (n, FuncT paramFunc t) : paramTypes
    where paramFunc  = map (\(Parameter ty _) -> ty) ps
          paramTypes = map (\(Parameter ty st) -> (st,ty)) ps


checkMethod :: MethodDecl -> TypeStateM MethodDecl
checkMethod (MethodDecl v t s ps stmts) = do
    typedStmts <- checkStmt stmts
    locals <- gets localTypeset
    fields <- gets fieldTypeset
    classT <- gets classType
    return $ MethodDecl v t s ps typedStmts


checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (Block stmts)                     = mapM checkStmt stmts >>= \bT -> return $ TypedStmt (Block bT) (getTypeS bT)
checkStmt (ReturnStmt e)                    = checkExpr e >>= \eT -> return $ TypedStmt (ReturnStmt eT) (getTypeE eT)
checkStmt (WhileStmt e stmts)               = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt stmts
    return $ TypedStmt (WhileStmt eTyped stmtsTyped) $ getTypeE eTyped -- TODO: Hier Typ vom BLock
checkStmt v@(LocalVarDeclStmt t s Nothing)          = do
    locals <- gets localTypeset
    modify (\state -> state {localTypeset = (s,t):locals})
    return $ TypedStmt (LocalVarDeclStmt t s Nothing) t
checkStmt v@(LocalVarDeclStmt t s (Just e))          = do
    locals <- gets localTypeset
    modify (\state -> state {localTypeset = (s,t):locals})
    eTyped <- checkTypeExpr t e
    return $ TypedStmt (LocalVarDeclStmt t s (Just eTyped)) t
checkStmt (IfElseStmt e bs Nothing)         = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt bs
    return $ TypedStmt (IfElseStmt eTyped stmtsTyped Nothing) VoidT
checkStmt (IfElseStmt e bs1 (Just bs2))      = do
    eT   <- checkTypeExpr BoolT e  
    bsT1 <- checkStmt bs1
    bsT2 <- checkStmt bs2
    return $ TypedStmt (IfElseStmt eT bsT1 (Just bsT2)) VoidT
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seT -> return $ TypedStmt (StmtExprStmt seT) VoidT
checkStmt s                                 = return $ TypedStmt s VoidT
checkStmt _                                 = error "checkStmt called on already typed Expression"

checkTypeExpr :: Type -> Expression -> TypeStateM Expression
checkTypeExpr t e = checkExpr e >>= \typed -> if t == getTypeE typed
    then return typed
    else error "checkTypeExpr error" -- #TODO: passende Error messages

-- TODO: alle Expr typen 
-- TODO: evtl. zu haskell typen? 
checkExpr :: Expression -> TypeStateM Expression
checkExpr ThisExpr                  = gets classType >>= \t -> return $ TypedExpr ThisExpr t
checkExpr SuperExpr                 = return $ TypedExpr SuperExpr VoidT -- #TODO: ändern zu ?
checkExpr (LocalOrFieldVarExpr i)   = checkIdentifier i
checkExpr v@(FieldVarExpr i)      = checkIdentifier i -- #TODO: korrigieren
checkExpr v@(LocalVarExpr i)      = checkIdentifier i -- #TODO: korrigieren
checkExpr (InstVarExpr e s)       = checkExpr e >>= \eTyped -> return $ TypedExpr (InstVarExpr eTyped s) StringT -- #TODO: korrigieren, was für ein Typ?
checkExpr (UnaryOpExpr op e)        = checkUnary op e
checkExpr (BinOpExpr e1 op e2)      = checkBinary e1 op e2
checkExpr e@(IntLitExpr _)          = return $ TypedExpr e IntT
checkExpr e@(BoolLitExpr _)         = return $ TypedExpr e BoolT
checkExpr e@(CharLitExpr _)  =      return $ TypedExpr e CharT -- #TODO: evtl nochmal iwo prüfen, das char
checkExpr e@(StringLitExpr _)       = return $ TypedExpr e StringT
checkExpr Null                      = return $ TypedExpr Null VoidT
checkExpr (StmtExprExpr se)         = checkStmtExpr se >>= \seTyped -> return $ TypedExpr (StmtExprExpr seTyped) (getTypeSE seTyped)
checkExpr _                         = error "checkExpr called on already typed Expression"

-- #TODO: lookup noch spezifizieren, ob wirklich in jeweiliger FUnktion
checkIdentifier :: String -> TypeStateM Expression
checkIdentifier s = do
    state <- get
    let localSet = localTypeset state
    let fieldSet = fieldTypeset state
    case lookup s localSet of
        Just localT -> return $ TypedExpr (LocalVarExpr s) localT
        _      -> case lookup s fieldSet of
                    Just fieldT -> return $ TypedExpr (FieldVarExpr s) fieldT
                    _           -> error "Vars müssen deklariert werden" -- #TODO: schöner

-- #TODO: evtl. umschreiben
checkUnary :: UnaryOperator -> Expression -> TypeStateM Expression
checkUnary Not e  = checkTypeExpr BoolT e >>= \eT -> return $ TypedExpr (UnaryOpExpr Not eT) BoolT
checkUnary op  e  = checkTypeExpr IntT  e >>= \eT -> return $ TypedExpr (UnaryOpExpr op eT ) IntT


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
    _           -> checkIntBinary IntT -- Plus, Minus, Times & Divide
    where checkIntBinary t    = checkSameExpr e1 e2 IntT  >>= result t
          checkBoolBinary     = checkSameExpr e1 e2 BoolT >>= result BoolT
          checkCompBinary     = checkSameExpr e1 e2 VoidT >>=  result BoolT
          result t (eT1, eT2) = return $ TypedExpr (BinOpExpr eT1 op eT2) t

checkSameExpr :: Expression  -> Expression -> Type -> TypeStateM (Expression, Expression)
checkSameExpr e1 e2 t = do
    e1T <- checkExpr e1 
    e2T <- checkExpr e2
    let equal = getTypeE e1T == getTypeE e2T        -- Are ExprTypes equal?
    if equal && (t == getTypeE e1T || t == VoidT)   -- equal & Both have required type
        then return (e1T,e2T) 
        else semanticsError "checkSameExpr" $ show e1T ++ " & " ++ show e2T ++ ", with Type " ++ show t ++ ", but type e1, e2: " ++ show (getTypeE e1T) ++ " " ++ show (getTypeE e2T) 


-- type checking statements & wraping StmtExpr into TypedStmtExpr
checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr se@(AssignmentStmt _ _) = checkAssign se
checkStmtExpr (NewExpression n)       = checkNew n
checkStmtExpr (MethodCall m)          = checkMethodCall m >>= \mTyped -> return $ TypedStmtExpr (MethodCall mTyped) VoidT -- #TODO: Anzahl und Art Parameter checken
checkStmtExpr _                       = error "checkStmtExpr called on already typed Stmt"



-- #TODO: AssignmentStmt _> AssignStmt rename
checkAssign :: StmtExpr -> TypeStateM StmtExpr
checkAssign (AssignmentStmt e1 e2) = do
    e1T <- checkExpr e1 
    e2T <- checkExpr e2
    -- #TODO: e1 muss var sein? und als Var it jeweiligen Typ abspeichern
    return $ TypedStmtExpr (AssignmentStmt e1T e2T) (getTypeE e2T)

checkNew :: NewExpr -> TypeStateM StmtExpr
checkNew (NewExpr cn es) = do
    cT <- gets classType
    let cT' = (\(NewTypeT n) -> n) cT
    let correctType = cT' == cn
    esT <- mapM checkExpr es
    eTyped      <- mapM checkExpr es
    return $ TypedStmtExpr (NewExpression(NewExpr cn eTyped)) cT
    -- #TODO: NewType Datentyp evtl anders?

checkMethodCall :: MethodCallExpr -> TypeStateM MethodCallExpr
checkMethodCall (MethodCallExpr e s es) = do
    eT <- checkExpr e
    -- #TODO: check if e is Object
    -- #TODO: check if Method is given in Obj
    -- #TODO: check if es are needed params (number of params and their type)
    eTyped  <- checkExpr e
    esTyped <- mapM checkExpr es
    return $ MethodCallExpr eTyped s esTyped

-- checkBlockStmt ::

getTypeE :: Expression -> Type
getTypeE (TypedExpr _ t) = t
getTypeE _               = error "blub"

getTypeSE :: StmtExpr -> Type
getTypeSE (TypedStmtExpr _ t) = t
getTypeSE _                    = error "blub"

getTypeS :: StmtExpr -> Type
getTypeS (TypedStmt _ t) = t
getTypeS _                    = error "blub"

-- Debug Helper 

semanticsError :: String -> String -> a
semanticsError s1 s2 = error $ "error in function " ++ s1 ++ "\ncalles on " ++ s2