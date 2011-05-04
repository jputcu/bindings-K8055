module Bindings.K8055.DigitalIn (  
  DigitalInput(..),
  readDigitalChannel,
  readAllDigital
  ) where

import Data.Word
import Foreign.C

data DigitalInput
  = DigitalIn1
  | DigitalIn2
  | DigitalIn3
  | DigitalIn4
  | DigitalIn5
  | DigitalIn6
  | DigitalIn7
  | DigitalIn8
    
digitalInputId :: Num a => DigitalInput -> a
digitalInputId input =
  case input of
    DigitalIn1 -> 1
    DigitalIn2 -> 2
    DigitalIn3 -> 3
    DigitalIn4 -> 4
    DigitalIn5 -> 5
    DigitalIn6 -> 6
    DigitalIn7 -> 7
    DigitalIn8 -> 8


foreign import stdcall unsafe "ReadDigitalChannel"
  c_ReadDigitalChannel :: CInt -> IO CInt

-- | Reads the status of the input channel
readDigitalChannel :: DigitalInput -> IO Word8
readDigitalChannel input = do
  res <- c_ReadDigitalChannel (digitalInputId input)
  return $ fromIntegral res


foreign import stdcall unsafe "ReadAllDigital"
  c_ReadAllDigital :: IO CInt

-- | Reads the status of all the input channels
readAllDigital :: IO Word8
readAllDigital = do
  res <- c_ReadAllDigital
  return $ fromIntegral res

