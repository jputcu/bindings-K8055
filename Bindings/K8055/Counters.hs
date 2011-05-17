module Bindings.K8055.Counters (
  Counter(..),
  resetCounter,
  readCounter,
  setCounterDebounceTime) where


import Foreign.C


data Counter
  = Counter1
  | Counter2

counterId :: Num a => Counter -> a
counterId cnt =
  case cnt of
    Counter1 -> 1
    Counter2 -> 2


foreign import stdcall unsafe "ResetCounter"
  c_ResetCounter :: CInt -> IO ()

-- | Resets the 16 bit pulse counter number 1 or counter number 2
resetCounter :: Counter -> IO ()
resetCounter cnt =
  c_ResetCounter (counterId cnt)


foreign import stdcall unsafe "ReadCounter"
  c_ReadCounter :: CInt -> IO CInt

-- | Reads the content of the pulse counte rnumber 1 or counter number 2
readCounter :: Counter -> IO Int
readCounter cnt = do
  val <- c_ReadCounter (counterId cnt)
  return $ fromIntegral val


foreign import stdcall unsafe "SetCounterDebounceTime"
  c_SetCounterDebounceTime :: CInt -> CInt -> IO ()

-- | Sets the debounce time to the pulse counter
setCounterDebounceTime :: Counter
                          -> Int    -- ^ 0 - 5000
                          -> IO ()
setCounterDebounceTime cnt debounce =
  c_SetCounterDebounceTime (counterId cnt) (fromIntegral debounce)
