import Control.Monad.State


import Syntax

import ClassFormat

import Control.Monad.State (gets, modify, State, runState)
import Control.Monad.State (liftIO)
import Data.Typeable
import Parser (parse)
import Data.List (intercalate)


import Control.Monad (when)
import Control.Monad.State (gets, get, modify, State, runState)
import Debug.Trace (traceShow)

data ThisListState = ThisListState { bspL :: [Int] }
    deriving Show

type ConstantpoolStateM = State ThisListState

-- Define the state type
type ListState = State ThisListState ()

-- Function to add an element to the list
addElement :: Int -> ListState
addElement x = do
    bs <- gets bspL
    modify (\s -> s { bspL = bs ++ [x]})

-- Function to add multiple elements to the list
addElements :: [Int] -> ListState
addElements xs = do
    bs <- gets bspL
    modify (\s -> s { bspL =  bs ++ xs })

-- Example usage
main :: IO ()
main = do
  -- Run the stateful computation starting from an initial state of an empty list
  let (state, finalList) = runState (do
        addElement 1
        addElement 2
        addElements [3, 4, 5]
        ) $ ThisListState []

  putStrLn $ "Final List: " ++ show finalList
  putStrLn $ "Final State: " ++ show state
