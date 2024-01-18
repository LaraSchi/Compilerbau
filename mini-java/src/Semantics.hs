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

fillTypeSetFields :: [FieldDecl] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {fieldTypeset = types}) 
    where types = map (\(FieldDecl t s) -> (s,t)) fds

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
    typedStmts <- mapM checkStmt stmts
    locals <- gets localTypeset
    fields <- gets fieldTypeset
    classT <- gets classType
    return $ MethodDecl v t s ps typedStmts


checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (ReturnStmt e)                    = checkExpr e >>= \eT -> return $ TypedStmt (ReturnStmt eT) (getTypeE eT)
checkStmt (WhileStmt e stmts)               = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- mapM checkStmt stmts
    return $ TypedStmt (WhileStmt eTyped stmtsTyped) VoidT
checkStmt v@(LocalVarDeclStmt t s)          = do
    locals <- gets fieldTypeset
    modify (\state -> state {fieldTypeset = (s,t):locals}) 
    return $ TypedStmt v t
checkStmt (IfElseStmt e bs Nothing)         = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- mapM checkStmt bs
    return $ TypedStmt (IfElseStmt eTyped stmtsTyped Nothing) VoidT
checkStmt (IfElseStmt e bs1 (Just bs2))      = do
    eT   <- checkTypeExpr BoolT e  
    bsT1 <- mapM checkStmt bs1
    bsT2 <- mapM checkStmt bs2
    return $ TypedStmt (IfElseStmt eT bsT1 (Just bsT2)) VoidT
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seT -> return $ TypedStmt (StmtExprStmt seT) VoidT
checkStmt _                                 = error "checkStmt called on already typed Expression"

checkTypeExpr :: Type -> Expression -> TypeStateM Expression
checkTypeExpr t e = checkExpr e >>= \typed -> if t == getTypeE typed
    then return typed
    else error "checkTypeExpr error" -- #TODO: passende Error messages

-- TODO: alle Expr typen 
-- TODO: evtl. zu haskell typen? 
checkExpr :: Expression -> TypeStateM Expression
checkExpr ThisExpr             = gets classType >>= \t -> return $ TypedExpr ThisExpr t
checkExpr SuperExpr            = return $ TypedExpr SuperExpr VoidT -- #TODO: ändern zu ?
checkExpr (IdentifierExpr i)   = checkIdentifier i
checkExpr v@(InstVar _ _)      = return $ TypedExpr v StringT
checkExpr (UnaryOpExpr op e)   = checkUnary op e
checkExpr (BinOpExpr e1 op e2) = checkBinary e1 op e2
checkExpr e@(IntLitExpr _)     = return $ TypedExpr e IntT
checkExpr e@(BoolLitExpr _)    = return $ TypedExpr e BoolT
checkExpr e@(CharLitExpr [_])  = return $ TypedExpr e CharT -- #TODO: evtl nochmal iwo prüfen, das char
checkExpr e@(StringLitExpr _)  = return $ TypedExpr e StringT
checkExpr Null                 = return $ TypedExpr Null VoidT
checkExpr (StmtExprExpr se)    = checkStmtExpr se >>= \seTyped -> return $ TypedExpr (StmtExprExpr seTyped) (getTypeSE seTyped)
checkExpr _                    = error "checkExpr called on already typed Expression"
 
checkIdentifier :: String -> TypeStateM Expression
checkIdentifier s = do
    state <- get
    local <- gets localTypeset
    let typeSet = localTypeset state ++ fieldTypeset state
    case lookup s $ localTypeset state ++ fieldTypeset state of
        Just t -> return $ TypedExpr (IdentifierExpr s) t
        _      -> return $ TypedExpr (IdentifierExpr s) VoidT -- #TODO: stimmt das?

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
        then return (e1,e2) 
        else semanticsError "checkSameExpr" $ show e1T ++ " & " ++ show e2T ++ ", with Type " ++ show t ++ ", but type e1, e2: " ++ show (getTypeE e1T) ++ " " ++ show (getTypeE e2T) 


-- type checking statements & wraping StmtExpr into TypedStmtExpr
checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr se@(AssignmentStmt _ _) = checkAssign se
checkStmtExpr (NewExpression n)       = checkNew n
checkStmtExpr (MethodCall m)          = checkMethodCall m >>= \mTyped -> return $ TypedStmtExpr (MethodCall mTyped) VoidT -- #TODO: Anzahl und Art Parameter checken
checkStmtExpr _                       = error "checkStmtExpr called on already typed Stmt"



-- #TODO: AssignmentStmt _> AssignStmt rename
checkAssign :: StmtExpr -> TypeStateM StmtExpr
checkAssign (AssignmentStmt var@(InstVar _ s) e) = do
    state <- get
    case lookup s $ localTypeset state ++ fieldTypeset state of
        Nothing -> error $ s ++ " has not been initialized" 
        Just t  -> checkTypeExpr t e >>= \eT -> return $ TypedStmtExpr (AssignmentStmt var eT) t
    -- #TODO: check if object v has attribut s
checkAssign t@(AssignmentStmt _ _)             = do
    return $ TypedStmtExpr t VoidT
    -- #TODO: check if e has same type as v
    -- #TODO: StmtExpr has type VoidT 
checkAssign _                                = error "checkAssign is called on a StmtExpr, which is not AssignmentStmt"

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


getTypeE :: Expression -> Type
getTypeE (TypedExpr _ t) = t
getTypeE _               = error "blub"

getTypeSE :: StmtExpr -> Type
getTypeSE (TypedStmtExpr _ t) = t
getTypeSE _                    = error "blub"

-- Debug Helper 

semanticsError :: String -> String -> a
semanticsError s1 s2 = error $ "error in function " ++ s1 ++ "\ncalles on " ++ s2