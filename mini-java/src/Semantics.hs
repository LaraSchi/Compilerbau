module Semantics
    ( checkSemantics
    ) where

import Syntax

checkSemantics :: Program -> Either String Program
checkSemantics = Right


-- #TODO: 
{- 
    - rekursive typisierungsfunktion
    - alle Methoden/Variabeln klar deklariert?
    - erster Plan: State Monad beim rekursiven Typchecken, alle 
    Variabeln und Methoden zwischenspeichern zum typchecken?
    - StateMonad (Menge Typnamen O = {id:ty})
 -}