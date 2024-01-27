import Control.Monad.State

-- Define a stateful computation that increments the counter and returns the current counter value
incrementAndGet :: State Int Int
incrementAndGet = do
  currentState <- get        -- Get the current state (counter)
  put (currentState + 1)    -- Increment the counter
  return currentState        -- Return the original counter value

-- Example usage
main :: IO ()
main = do
  let initialState = 0         -- Initial state (counter is 0)
      (result, finalState) = runState incrementAndGet initialState

  putStrLn $ "Result: " ++ show result          -- Result is the original counter value
  putStrLn $ "Final State: " ++ show finalState  -- Final state is the incremented counter