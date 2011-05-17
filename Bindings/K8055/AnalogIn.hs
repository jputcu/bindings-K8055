module Bindings.K8055.AnalogIn (
  AnalogInput(..),
  readAnalogChannel,
  readAllAnalog
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Word

data AnalogInput
  = AnalogIn1
  | AnalogIn2

analogInputId :: Num a => AnalogInput -> a
analogInputId input =
  case input of
    AnalogIn1 -> 1
    AnalogIn2 -> 2


foreign import stdcall unsafe "ReadAnalogChannel"
  c_ReadAnalogChannel :: CInt -> IO CInt

-- | Reads the status of one analogue input-channel
readAnalogChannel :: AnalogInput -> IO Word8
readAnalogChannel channel = do
  res <- c_ReadAnalogChannel (analogInputId channel)
  return $ fromIntegral res


foreign import stdcall unsafe "ReadAllAnalog"
  c_ReadAllAnalog :: Ptr CInt -> Ptr CInt -> IO ()

-- | Reads the status of both analogue input-channels
readAllAnalog :: IO (Word8, Word8)
readAllAnalog = do
  alloca $ \ a1 ->
    alloca $ \ a2 -> do
      c_ReadAllAnalog a1 a2
      a1' <- peek a1
      a2' <- peek a2
      return (fromIntegral a1', fromIntegral a2')


