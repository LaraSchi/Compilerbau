module Semantics ( checkSemantics ) where

import Syntax
import Control.Monad.State (gets, get, modify, State, runState)
import Data.List

--  state needed for type checking
data TypeState = TypeState { classType        :: Type,              -- Type of current class
                             localTypeset     :: [(String, Type)],  -- local vars + types
                             fieldTypeset     :: [(String, Type)],  -- field vars types & function types 
                             errors           :: [String] }         -- list of errors 
                             deriving Show 

type TypeStateM = State TypeState

-- Entry point for semantic analysis:
-- Program as input, returns a tuple containing the program and eventually a list of errors.
-- runState to execute the semantic analysis (checkSProg) with an initial state.
checkSemantics :: Program -> (Program, [String])
checkSemantics p = fmap errors . runState (checkSProg p) $ TypeState VoidT [] [] []

-- Traverses the AST, checks class members, and updates the state accordingly.
checkSProg :: Program -> TypeStateM Program
checkSProg p@(Program (Class n fd md) _) = do
        modify $ \s -> s {classType = NewTypeT n}
        fillTypeSetFields fd             -- caches FieldVars
        fillTypesSetMethod md            -- caches method signatures
        mdTyped <- mapM checkMethod md
        errors  <- gets errors
        let checkSuccess = null errors   -- no errors -> semantic check was successful 
        return (Program (Class n fd mdTyped) checkSuccess)

-- Caches name and type of FieldVar declarations in State Monad as Tuple (name, type)
fillTypeSetFields :: [Field] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {fieldTypeset = typesOf fds})
    where typesOf :: [Field] -> [(String, Type)]
          typesOf []                    = []
          typesOf (FieldDecl t s me:ys) = (s,t):typesOf ys

-- Populates the field type set with method signatures.
fillTypesSetMethod :: [MethodDecl] -> TypeStateM ()
fillTypesSetMethod mds = gets fieldTypeset >>= \ts -> modify (\s -> s {fieldTypeset = map buildFuncType mds ++ ts})

buildFuncType :: MethodDecl -> (String, Type)
buildFuncType (MethodDecl _ t n ps _) = (n, FuncT paramFunc t)
    where paramFunc  = map (\(Parameter ty _) -> ty) ps

-- Performs type checking on a method declaration.
-- The type state is modified to include the local variables and their types (localTypeset) 
-- and then performs type checking on the method's statements.
checkMethod :: MethodDecl -> TypeStateM MethodDecl
checkMethod (MethodDecl v t s ps stmts) = do
    let paramTypes = map (\(Parameter ty st) -> (st,ty)) ps
    modify (\state -> state {localTypeset = (s,t):paramTypes})
    typedStmts <- checkStmt stmts
    if getTypeS typedStmts == t
    then return $ MethodDecl v t s ps typedStmts
    else do 
        errorCall "funcTyErr" s
        let errorType = changeTypeS (wrongType (show (getTypeS typedStmts))) typedStmts
        return $ MethodDecl v t s ps errorType

-- Performs type checking on a block of statements.
-- eturns a typed block statement, indicating either VoidT if there are no return statements 
-- or the return type of the method if all paths have consistent return types.
checkBlock :: [Stmt] -> TypeStateM Stmt
checkBlock stmts = do
    bT <- mapM checkStmt stmts
    let types = filter (/= VoidT) $ map getTypeS bT
    case (null types, allEq types) of
        (True, _  ) -> return $ TypedStmt (Block bT) VoidT
        (_  , True) -> return $ TypedStmt (Block bT) (head types)
        _           -> errorCall "blockTyErr"  "" >> return (TypedStmt (Block bT) (NewTypeT $ NewType "xVoidT"))

-- Performs type checking on a statement.
-- Depending on the type of statement, it calls corresponding helper functions for type checking.
checkStmt :: Stmt -> TypeStateM Stmt
checkStmt (Block stmts)                     = checkBlock stmts
checkStmt (ReturnStmt e)                    = checkExpr e >>= \eT -> return $ TypedStmt (ReturnStmt eT) (getTypeE eT)
checkStmt (WhileStmt e stmts)               = checkWhileStmt e stmts
checkStmt v@(LocalVarDeclStmt {})           = checkVarDeclStmt v
checkStmt (IfElseStmt e bs Nothing)         = checkIfStmt e bs
checkStmt (IfElseStmt e bs1 (Just bs2))     = checkIfELseStmt e bs1 bs2
checkStmt (StmtExprStmt se)                 = checkStmtExpr se >>= \seT -> return $ TypedStmt (StmtExprStmt seT) VoidT-- (getTypeSE seT)
checkStmt s                                 = return $ TypedStmt s VoidT

-- Handles the type checking for local variable declarations within the method.
-- Updates the localTypeset in the state to include the newly declared variables and their types.
checkVarDeclStmt :: Stmt -> TypeStateM Stmt
checkVarDeclStmt (LocalVarDeclStmt t s maybeE) = do
    locals <- gets localTypeset
    modify (\state -> state {localTypeset = (s,t):locals})
    case maybeE of
        Just e  -> checkTypeExpr t e >>= \eTy -> return $ TypedStmt (LocalVarDeclStmt t s (Just eTy)) VoidT
        Nothing -> return $ TypedStmt (LocalVarDeclStmt t s Nothing) VoidT

-- Performs type checking on an if statement.
-- Checks if the condition is of type BoolT, then checks the body of the if statement for type correctness.
-- Returns a typed statement with an IfElseStmt containing the typed condition and body.
checkIfStmt :: Expression -> Stmt -> TypeStateM Stmt
checkIfStmt e stmts =  do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt stmts
    return $ TypedStmt (IfElseStmt eTyped stmtsTyped Nothing) (getTypeS stmtsTyped)

-- Performs type checking on a while statement.
-- Verifies if the condition is of type BoolT, then checks the body of the while statement for type correctness.
-- Returns a typed statement with a WhileStmt containing the typed condition and body.
checkWhileStmt :: Expression -> Stmt -> TypeStateM Stmt
checkWhileStmt e stmts = do
    eTyped     <- checkTypeExpr BoolT e 
    stmtsTyped <- checkStmt stmts
    return $ TypedStmt (WhileStmt eTyped stmtsTyped) $ getTypeS stmtsTyped

-- Performs type checking on an expression with an expected type.
-- Checks if the type of the expression matches the expected type.
-- If the types mismatch, raises an error with details of the expression and the expected type.
checkTypeExpr :: Type -> Expression -> TypeStateM Expression
checkTypeExpr t e = checkExpr e >>= \typed -> if t == getTypeE typed
    then return typed
    else error $ "The expression" ++ show typed ++ "does not have the expected type " ++ show t ++  "!"

-- Performs type checking on an expression.
-- Checks various kinds of expressions including variables, literals, unary and binary operations.
-- Returns a typed expression with the inferred type.
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

-- Performs type checking on an if-else statement.
-- Checks if the condition is of type BoolT, then checks both the 'if' and 'else' branches for consistency in return types.
-- If the types mismatch, records an error with details of the types involved.
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

-- Performs type checking on an instance variable expression.
-- Checks if the given variable exists in the field typeset and returns its type.
-- If the variable does not exist, raises an error.
checkInstVarExpr :: String -> TypeStateM Type
checkInstVarExpr s = do
    fields <- gets fieldTypeset
    case filter ((==s).fst) fields of
        [(_,t)]  -> return t 
        _        -> errorCall "varUnknown" s >> return (wrongType "error")

-- Performs type checking on unary operations.
-- Checks if the operand has the appropriate type for the given operator.
-- Returns a typed expression with the resulting type of the operation.
checkUnary :: UnaryOperator -> Expression -> TypeStateM Expression
checkUnary Not e  = checkTypeExpr BoolT e >>= \eT -> return $ TypedExpr (UnaryOpExpr Not eT) BoolT
checkUnary op  e  = checkTypeExpr IntT  e >>= \eT -> return $ TypedExpr (UnaryOpExpr op eT ) IntT

-- Performs identifier type checking.
-- Checks if the identifier corresponds to a local variable or a field variable,
-- then returns a typed expression with the appropriate type.
checkIdentifier :: String -> TypeStateM Expression
checkIdentifier s = do
    state <- get
    let (localSet, fieldSet) = (localTypeset state,fieldTypeset state)
    case (lookup s localSet, lookup s fieldSet) of
        (Just localT, _)   -> return $ TypedExpr (LocalVarExpr s) localT
        (_, Just fieldT)   -> return $ TypedExpr (FieldVarExpr s) fieldT
        (Nothing, Nothing) -> errorCall "varUnknown"  s >> return (TypedExpr (FieldVarExpr s) (wrongType "unknown"))

-- Performs type checking on field variables.
-- Checks if the field variable exists in the field typeset and returns its type.
-- If the variable does not exist, raises an error.
checkFieldVar :: String -> TypeStateM Expression
checkFieldVar s = do
    field <- gets fieldTypeset
    case lookup s field of
        Just fieldT -> return $ TypedExpr (FieldVarExpr s) fieldT
        _           -> errorCall "varUnknown" s >> return (TypedExpr (FieldVarExpr s) (wrongType "unknown"))

-- Performs type checking on binary operations.
-- Determines the appropriate type of the operation based on the operator and the types of the operands.
-- Returns a typed expression with the resulting type of the operation.
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

-- Performs type checking on two expressions for binary operations.
-- Checks if the expressions have compatible types for the operation.
-- If the types do not match, raises an error.
checkSameExpr :: Expression  -> Expression -> Type -> TypeStateM (Expression, Expression)
checkSameExpr e1 e2 t = do
    e1T <- checkExpr e1 
    e2T <- checkExpr e2
    let (ty1, ty2) = (getTypeE e1T, getTypeE e2T )
    let equal = ty1 == ty2                      -- Are ExprTypes equal?
    if equal && (t == ty1 || t == VoidT)        -- equal & Both have required type
        then return (e1T,e2T) 
        else do
            errorCall "OpTyErr" "" 
            let (ty1', ty2') = (wrongType (show ty1), wrongType (show ty2))
            return (changeTypeE ty1' e1T,changeTypeE ty2' e2T) 


-- Performs type checking on statement expressions.
-- Wraps the typed statement expression into a TypedStmtExpr.
checkStmtExpr :: StmtExpr -> TypeStateM StmtExpr
checkStmtExpr (AssignmentStmt e1 e2) = checkAssign e1 e2
checkStmtExpr (NewExpression n)      = checkNew n
checkStmtExpr (MethodCall m)         = checkMethodCall m >>= \(mT,rty) -> return $ TypedStmtExpr (MethodCall mT) rty
checkStmtExpr _                      = error "checkStmtExpr called on already typed Stmt"

-- Performs type checking on assignment statements.
-- Checks the types of the expressions being assigned and returns a typed assignment statement.
checkAssign :: Expression -> Expression -> TypeStateM StmtExpr
checkAssign e1 e2 = checkExpr e1 >>= \e1T -> checkExpr e2 >>= \e2T -> return $ TypedStmtExpr (AssignmentStmt e1T e2T) VoidT

-- Performs type checking on 'new' expressions.
-- Checks if the class type matches the expected type and returns a typed 'new' expression.
checkNew :: NewExpr -> TypeStateM StmtExpr
checkNew (NewExpr cn es) = do
    cT <- gets classType
    let cT' = (\(NewTypeT n) -> n) cT
    let correctType = cT' == cn
    eTyped  <- mapM checkExpr es
    return $ TypedStmtExpr (NewExpression(NewExpr cn eTyped)) (NewTypeT cn) 

-- Performs type checking on a method call expression.
-- Checks if the method exists in the field typeset, verifies the types of its arguments,
-- and returns the resulting type of the method call.
checkMethodCall :: MethodCallExpr -> TypeStateM (MethodCallExpr,Type)
checkMethodCall (MethodCallExpr e s es) = do
    field   <- gets fieldTypeset
    eTyped  <- checkExpr e
    esTyped <- mapM checkExpr es
    let funcType = filter ((==s).fst) field
    if null funcType 
    then errorCall "funcUnknown" s >> return (MethodCallExpr eTyped s esTyped,wrongType "error")
    else if getParams funcType == map getTypeE esTyped 
        then do
            let (params, resultType) = getFuncTypes (snd (head funcType))
            return (MethodCallExpr eTyped s esTyped,resultType)
        else errorCall "paramTyErr" s >> return (MethodCallExpr eTyped s esTyped,wrongType "error")

------ Helper ------

-- Extracts the parameter types from a function type
getParams :: [(String, Type)] -> [Type]
getParams ((_,FuncT ts _):_) = ts
getParams _                  = error "getParams wurde auf falschem typen aufgerufen"

-- Extracts the parameter types and return type from a function type.
getFuncTypes :: Type -> ([Type],Type)
getFuncTypes (FuncT p r) = (p,r)
getFuncTypes _           = error "getFuncTypes wurde auf falschem typen aufgerufen"

-- Retrieves the type of an expression.
getTypeE :: Expression -> Type
getTypeE (TypedExpr _ t) = t
getTypeE t               = error $ "getTypeE error with:" ++ show t

-- Retrieves the type of a statement expression.
getTypeSE :: StmtExpr -> Type
getTypeSE (TypedStmtExpr _ t) = t
getTypeSE t                   = error $ "getTypeSE error with:" ++ show t

-- Retrieves the type of a statement.
getTypeS :: Stmt -> Type
getTypeS (TypedStmt _ t) = t
getTypeS t               = error $ "getTypeS error with:" ++ show t

-- helper function used to change the type of a statement.
-- Primarily used when type errors are detected to update the type of the statement accordingly.
changeTypeS :: Type -> Stmt -> Stmt
changeTypeS t' (TypedStmt s t) = TypedStmt s t'
changeTypeS _  s               = error $ "changeTypeS error with:" ++ show s

changeTypeE :: Type -> Expression -> Expression
changeTypeE t' (TypedExpr e t) = TypedExpr e t'
changeTypeE _  e               = error $ "changeTypeE error with:" ++ show e

-- Checks if all elements in a list are equal.
allEq :: Eq a => [a] -> Bool
allEq (x:xs) = all (== x) xs
allEq _      = True

-- Creates a type representing a wrong type error message based on the input string.
-- Used when type errors are detected during semantic analysis.
wrongType :: String -> Type
wrongType s = case stripPrefix "NewTypeT (NewType \"" s of
     Just ('x': rest)   -> NewTypeT $ NewType "xerror"
     Just rest          -> NewTypeT $ NewType ("x" ++ init (init rest))
     Nothing            -> NewTypeT $ NewType $ "x"++ s

-- Error Handling: Calls addError with predefined error messages based on the error type.
errorCall :: String -> String -> TypeStateM ()
errorCall "varUnknown"  x = addError $ "The variable " ++ show x ++ " is not known. Please declare before use!"
errorCall "funcUnknown" x = addError $ "The function " ++ show x ++ " is not known. Please declare before use!"
errorCall "funcTyErr"   x = addError $ "The function " ++ show x ++ " returns a different type than declared."
errorCall "paramTyErr"  x = addError $ "The function " ++ show x ++ " requires other parameters."
errorCall "blockTyErr"  x = addError "BlockStatement contains type error"
errorCall "ifElseTyErr" x = addError "The types of the if and else cases do not match!"
errorCall "OpTyErr"     x = addError "The Binary operator does not receive the correct types!"
errorCall error         _ = addError error

-- Adds an error message to the list of errors in the state, when type errors are detected during semantic analysis.
addError :: String -> TypeStateM ()
addError e = do
    es <- gets errors
    modify (\s -> s {errors = ("   - " ++ e) : es})