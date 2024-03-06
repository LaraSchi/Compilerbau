module Semantics ( checkSemantics ) where

import Syntax
import Control.Monad.State (gets, get, modify, State, runState)
import Control.Monad (when)
import Data.List

import Debug.Trace --Debugging, TODO: remove


data TypeState = TypeState { classType        :: Type,
                             localTypeset     :: [(String, Type)],
                             fieldTypeset     :: [(String, Type)],
                             errors           :: [String] }
                             deriving Show 

type TypeStateM = State TypeState

-- 
checkSemantics :: Program -> (Program, [String])
checkSemantics p = fmap errors . runState (checkSProg p) $ TypeState VoidT [] [] []

-- runs recursively through the AST to check types
checkSProg :: Program -> TypeStateM Program
checkSProg p@(Program (Class n fd md) _) = do
        modify $ \s -> s {classType = NewTypeT n}
        fillTypeSetFields fd             -- caches FieldVars
        fillTypesSetMethod md            -- caches method signatures
        mdTyped <- mapM checkMethod md
        errors  <- gets errors
        let checkSuccess = null errors   -- no errors -> semantic check was successful 
        return (Program (Class n fd mdTyped) checkSuccess)

-- caches name and type of FieldVar declarations in State Monad as (name, type)
fillTypeSetFields :: [Field] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {fieldTypeset = typesOf fds})
    where typesOf :: [Field] -> [(String, Type)]
          typesOf []                    = []
          typesOf (FieldDecl t s me:ys) = (s,t):typesOf ys
          typesOf (_:ys)                = typesOf ys

-- füllt Typen der Funktionen in FieldTypeSet
fillTypesSetMethod :: [MethodDecl] -> TypeStateM ()
fillTypesSetMethod mds = gets fieldTypeset >>= \ts -> modify (\s -> s {fieldTypeset = map buildFuncType mds ++ ts})

buildFuncType :: MethodDecl -> (String, Type)
buildFuncType (MethodDecl _ t n ps _) = (n, FuncT paramFunc t)
    where paramFunc  = map (\(Parameter ty _) -> ty) ps


checkMethod :: MethodDecl -> TypeStateM MethodDecl
checkMethod (MethodDecl v t s ps stmts) = do
    let paramTypes = map (\(Parameter ty st) -> (st,ty)) ps
    modify (\state -> state {localTypeset = (s,t):paramTypes})
    typedStmts <- checkStmt stmts
    if getTypeS typedStmts == t
    then return $ MethodDecl v t s ps typedStmts
    else do 
        errorCall "funcTyErr" s
        return $ MethodDecl v t s ps (changeTypeS (wrongType (show (getTypeS typedStmts))) typedStmts)


checkBlock :: [Stmt] -> TypeStateM Stmt
checkBlock stmts = do
    bT <- mapM checkStmt stmts
    let types = filter (/= VoidT) $ map getTypeS bT
    case (null types, allEq types) of
        (True, _  ) -> return $ TypedStmt (Block bT) VoidT
        (_  , True) -> return $ TypedStmt (Block bT) (head types)
        _           -> do
            errorCall "blockTyErr"  ""
            return $ TypedStmt (Block bT) (NewTypeT $ NewType "xVoidT")


checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (Block stmts)                     = checkBlock stmts
checkStmt (ReturnStmt e)                    = checkExpr e >>= \eT -> return $ TypedStmt (ReturnStmt eT) (getTypeE eT)
checkStmt (WhileStmt e stmts)               = checkWhileStmt e stmts
checkStmt v@(LocalVarDeclStmt {})           = checkVarDeclStmt v
checkStmt (IfElseStmt e bs Nothing)         = checkIfStmt e bs
checkStmt (IfElseStmt e bs1 (Just bs2))     = checkIfELseStmt e bs1 bs2
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seT -> return $ TypedStmt (StmtExprStmt seT) VoidT-- (getTypeSE seT)
checkStmt s                                 = return $ TypedStmt s VoidT
checkStmt _                                 = error "checkStmt called on already typed Expression"

checkVarDeclStmt :: Stmt -> TypeStateM Stmt
checkVarDeclStmt (LocalVarDeclStmt t s maybeE) = do
    locals <- gets localTypeset
    modify (\state -> state {localTypeset = (s,t):locals})
    case maybeE of
        Just e  -> checkTypeExpr t e >>= \eTy -> return $ TypedStmt (LocalVarDeclStmt t s (Just eTy)) VoidT
        Nothing -> return $ TypedStmt (LocalVarDeclStmt t s Nothing) VoidT


checkIfStmt :: Expression -> Stmt -> TypeStateM Stmt
checkIfStmt e stmts =  do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt stmts
    return $ TypedStmt (IfElseStmt eTyped stmtsTyped Nothing) (getTypeS stmtsTyped)

checkWhileStmt :: Expression -> Stmt -> TypeStateM Stmt
checkWhileStmt e stmts = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt stmts
    return $ TypedStmt (WhileStmt eTyped stmtsTyped) $ getTypeS stmtsTyped

checkTypeExpr :: Type -> Expression -> TypeStateM Expression
checkTypeExpr t e = checkExpr e >>= \typed -> if t == getTypeE typed
    then return typed
    else error $ "The expression" ++ show typed ++ "does not have the expected type " ++ show t ++  "!"

-- TODO: alle Expr typen 
checkExpr :: Expression -> TypeStateM Expression
checkExpr ThisExpr                  = gets classType >>= \t -> return $ TypedExpr ThisExpr t
checkExpr SuperExpr                 = return $ TypedExpr SuperExpr VoidT
checkExpr (LocalOrFieldVarExpr i)   = checkIdentifier i
checkExpr v@(FieldVarExpr i)        = checkFieldVar i
checkExpr v@(LocalVarExpr i)        = checkIdentifier i
checkExpr (InstVarExpr e s)         = checkExpr e >>= \eTyped -> checkInstVarExpr s >>= \sT -> return $ TypedExpr (InstVarExpr eTyped s) sT
checkExpr (UnaryOpExpr op e)        = checkUnary op e
checkExpr (BinOpExpr e1 op e2)      = checkBinary e1 op e2
checkExpr e@(IntLitExpr _)          = return $ TypedExpr e IntT
checkExpr e@(BoolLitExpr _)         = return $ TypedExpr e BoolT
checkExpr e@(CharLitExpr _)         = return $ TypedExpr e CharT
checkExpr e@(StringLitExpr _)       = return $ TypedExpr e StringT
checkExpr Null                      = return $ TypedExpr Null VoidT
checkExpr (StmtExprExpr se)         = checkStmtExpr se >>= \seTyped -> return $ TypedExpr (StmtExprExpr seTyped) (getTypeSE seTyped)
checkExpr _                         = error "checkExpr called on already typed Expression"

checkIfELseStmt :: Expression -> Stmt -> Stmt -> TypeStateM Stmt
checkIfELseStmt e bs1 bs2      = do
    eT   <- checkTypeExpr BoolT e  
    bsT1 <- checkStmt bs1
    bsT2 <- checkStmt bs2
    let (type1,type2) = (getTypeS bsT1, getTypeS bsT2)
    if type1 == type2
        then return $ TypedStmt (IfElseStmt eT bsT1 (Just bsT2)) type2
        else do
            errorCall "ifElseTyErr" ""
            let bsT1' = changeTypeS (wrongType (show type1)) bsT1
            let bsT2' = changeTypeS (wrongType (show type2)) bsT2
            return $ TypedStmt (IfElseStmt eT bsT1' (Just bsT2')) $ wrongType "error"

-- Blablabla
checkInstVarExpr :: String -> TypeStateM Type
checkInstVarExpr s = do
    fields <- gets fieldTypeset
    case filter ((==s).fst) fields of
        [(_,t)]  -> return t 
        _        -> error "Var existiert nicht"

-- Blablabla
checkUnary :: UnaryOperator -> Expression -> TypeStateM Expression
checkUnary Not e  = checkTypeExpr BoolT e >>= \eT -> return $ TypedExpr (UnaryOpExpr Not eT) BoolT
checkUnary op  e  = checkTypeExpr IntT  e >>= \eT -> return $ TypedExpr (UnaryOpExpr op eT ) IntT

checkIdentifier :: String -> TypeStateM Expression
checkIdentifier s = do
    state <- get
    let (localSet, fieldSet) = (localTypeset state,fieldTypeset state)
    case (lookup s localSet, lookup s fieldSet) of
        (Just localT, _)   -> return $ TypedExpr (LocalVarExpr s) localT
        (_, Just fieldT)   -> return $ TypedExpr (FieldVarExpr s) fieldT
        (Nothing, Nothing) -> errorCall "varUnknown"  s >> return (TypedExpr (FieldVarExpr s) (wrongType "unknown"))

checkFieldVar :: String -> TypeStateM Expression
checkFieldVar s = do
    field <- gets fieldTypeset
    case lookup s field of
        Just fieldT -> return $ TypedExpr (FieldVarExpr s) fieldT
        _           -> errorCall "varUnknown" s >> return (TypedExpr (FieldVarExpr s) (wrongType "unknown"))

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
    let (ty1, ty2) = (getTypeE e1T, getTypeE e2T )
    let equal = ty1 == ty2                      -- Are ExprTypes equal?
    if equal && (t == ty1 || t == VoidT)      -- equal & Both have required type
        then return (e1T,e2T) 
        else errorCall "OpTyErr" "" >> return (changeTypeE (wrongType (show ty1)) e1T,changeTypeE (wrongType (show ty2)) e2T) 


-- type checking statements & wraping StmtExpr into TypedStmtExpr
checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr se@(AssignmentStmt _ _) = checkAssign se
checkStmtExpr (NewExpression n)       = checkNew n
checkStmtExpr (MethodCall m)          = checkMethodCall m >>= \(mT,rty) -> return $ TypedStmtExpr (MethodCall mT) rty-- #TODO: Anzahl und Art Parameter checken
checkStmtExpr _                       = error "checkStmtExpr called on already typed Stmt"


checkAssign :: StmtExpr -> TypeStateM StmtExpr
checkAssign (AssignmentStmt e1 e2) = do
    e1T <- checkExpr e1 
    e2T <- checkExpr e2
    return $ TypedStmtExpr (AssignmentStmt e1T e2T) VoidT

checkNew :: NewExpr -> TypeStateM StmtExpr
checkNew (NewExpr cn es) = do
    cT <- gets classType
    let cT' = (\(NewTypeT n) -> n) cT
    let correctType = cT' == cn
    eTyped  <- mapM checkExpr es
    return $ TypedStmtExpr (NewExpression(NewExpr cn eTyped)) (NewTypeT cn) 

--TODO: mit checkNew zusammen
checkMethodCall :: MethodCallExpr -> TypeStateM (MethodCallExpr,Type)
checkMethodCall (MethodCallExpr e s es) = do
    field   <- gets fieldTypeset
    eTyped  <- checkExpr e
    esTyped <- mapM checkExpr es
    let funcType = filter ((==s).fst) field
    if null funcType 
    then do
        errorCall "funcUnknown" s
        return (MethodCallExpr eTyped s esTyped,wrongType "error")
    else if getParams funcType == map getTypeE esTyped 
        then do
            let (params, resultType) = getFuncTypes (snd (head funcType))
            return (MethodCallExpr eTyped s esTyped,resultType)
        else do
            errorCall "paramTyErr" s
            return (MethodCallExpr eTyped s esTyped,wrongType "error")

-- Helper

getParams :: [(String, Type)] -> [Type]
getParams ((_,FuncT ts _):_) = ts
getParams _                  = error "TODO"

getFuncTypes :: Type -> ([Type],Type)
getFuncTypes (FuncT p r) = (p,r)
getFuncTypes _           = error "getFuncTypes wurde auf falschem typen aufgerufen"

getTypeE :: Expression -> Type
getTypeE (TypedExpr _ t) = t
getTypeE t               = error $ "getTypeE error with:" ++show t

getTypeSE :: StmtExpr -> Type
getTypeSE (TypedStmtExpr _ t) = t
getTypeSE t                   = error $ "getTypeSE error with:" ++show t

getTypeS :: Stmt -> Type
getTypeS (TypedStmt _ t) = t
getTypeS t               = error $ "getTypeS error with:" ++show t

changeTypeS :: Type -> Stmt -> Stmt
changeTypeS t' (TypedStmt s t) = TypedStmt s t'
changeTypeS _  s               = error $ "changeTypeS error with:" ++show s

changeTypeE :: Type -> Expression -> Expression
changeTypeE t' (TypedExpr e t) = TypedExpr e t'
changeTypeE _  e               = error $ "changeTypeE error with:" ++show e

allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (== x) xs
allEq _      = True

wrongType :: String -> Type
wrongType s = case stripPrefix "NewTypeT (NewType \"" s of
     Just ('x': rest)   -> NewTypeT $ NewType "xerror"
     Just rest          -> NewTypeT $ NewType ("x" ++ init (init rest))
     Nothing            -> NewTypeT $ NewType $ "x"++ s

-- Error Handling:
errorCall :: String -> String -> TypeStateM ()
errorCall "varUnknown"  x = addError $ "The variable " ++ show x ++ " is not known. Please declare before use!"
errorCall "funcUnknown" x = addError $ "The function " ++ show x ++ " is not known. Please declare before use!"
errorCall "funcTyErr"   x = addError $ "The function " ++ show x ++ " returns a different type than declared."
errorCall "paramTyErr"  x = addError $ "The function " ++ show x ++ " requires other parameters."
errorCall "blockTyErr"  x = addError "BlockStatement contains type error"
errorCall "ifElseTyErr" x = addError "The types of the if and else cases do not match!"
errorCall "OpTyErr"     x = addError "The Binary operator does not receive the correct types!"
errorCall error         _ = addError error

addError :: String -> TypeStateM ()
addError e = do
    es <- gets errors
    modify (\s -> s {errors = ("   - " ++ e) : es})

-- TODO: remove 

-- Debug Helper 

semanticsError :: String -> String -> a
semanticsError s1 s2 = error $ "error in function " ++ s1 ++ "\ncalles on " ++ s2