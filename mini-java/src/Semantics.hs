module Semantics
    ( checkSemantics
    ) where

import Syntax
import Control.Monad.Except
import Control.Monad.State (gets, modify, State, MonadState (put, get), runState)


data TypeState = TypeState { name       :: String,
                             typeset    :: [(String, Type)],
                             errors     :: [String]}

type TypeStateM = State TypeState

checkSemantics :: Program -> Either String Program
checkSemantics p = case runTypeStateM p of
    (p',[]) -> Right p
    (p',es) -> Left $ head es -- #TODO: errors schön zu errormessage konkatinieren 


runTypeStateM :: Program -> (Program, [String])
runTypeStateM p = fmap errors . runState (checkSProg p) $ TypeState [] [] []

checkSProg :: Program -> TypeStateM Program
checkSProg p@(Program (Class (Newtype n) fd md)) = do
        modify $ \s -> s {name = n}
        fillTypeSetFields fd
        fillTypesSetMethod md
        -- checkMethods md
        return p


fillTypeSetFields :: [FieldDecl] -> TypeStateM ()
fillTypeSetFields fds = modify (\s -> s {typeset = types}) 
    where types = map (\(FieldDecl t s) -> (s,t)) fds

fillTypesSetMethod :: [MethodDecl] -> TypeStateM ()
fillTypesSetMethod mds = do
    ts <- gets typeset
    let funcTypes = map buildFuncType mds
    modify (\s -> s {typeset = ts ++ funcTypes})

buildFuncType :: MethodDecl -> (String, Type)
buildFuncType (MethodDecl _ t n ps _) = (n, Func param t)
    where param = map (\(Parameter ty st) -> ty) ps


{- checkMethods :: [MethodDecl] -> TypeStateM ()
checkMethods = checkStmts . map (\(MethodDecl _ _ _ _ stmts) -> checkStmts stmts) 

checkStmts :: [Stmt] -> TypeStateM Type
checkStmts = return Int
 -}


-- TODO: #error "should not appear"


-- #TODO: 
{- 
    - rekursive typisierungsfunktion
    - alle Methoden/Variabeln klar deklariert?
    - erster Plan: State Monad beim rekursiven Typchecken, alle 
    Variabeln und Methoden zwischenspeichern zum typchecken?
    - StateMonad (Menge Typnamen O = {id:ty})
 -}