module Bindings.K8055.AnalogOut (
  AnalogOutput(..),
  outputAnalogChannel,
  outputAllAnalog,
  clearAnalogChannel,
  clearAllAnalog,
  setAnalogChannel,
  setAllAnalog
  ) where

import Foreign.C
import Data.Word

data AnalogOutput
  = AnalogOut1
  | AnalogOut2

analogOutputId :: Num a => AnalogOutput -> a
analogOutputId output =
  case output of
    AnalogOut1 -> 0
    AnalogOut2 -> 1

foreign import stdcall unsafe "OutputAnalogChannel"
  c_OutputAnalogChannel :: CInt -> CInt -> IO ()

-- | Sets the analogue output channel according to the data
outputAnalogChannel :: AnalogOutput -> Word8 -> IO ()
outputAnalogChannel channel dat = do
  c_OutputAnalogChannel (analogOutputId channel) (fromIntegral dat)


foreign import stdcall unsafe "OutputAllAnalog"
  c_OutputAllAnalog :: CInt -> CInt -> IO ()

-- | Sets both analogue output channels according to the data
outputAllAnalog :: Word8 -> Word8 -> IO ()
outputAllAnalog val1 val2 = do
  c_OutputAllAnalog (fromIntegral val1) (fromIntegral val2)


foreign import stdcall unsafe "ClearAnalogChannel"
  c_ClearAnalogChannel :: CInt -> IO ()

-- | Sets the analogue output channel to minimum
clearAnalogChannel :: AnalogOutput -> IO ()
clearAnalogChannel channel = do
  c_ClearAnalogChannel (analogOutputId channel)


foreign import stdcall unsafe "ClearAllAnalog"
  c_ClearAllAnalog :: IO ()

-- | Sets all analogue output channels to minimum
clearAllAnalog :: IO ()
clearAllAnalog = c_ClearAllAnalog


foreign import stdcall unsafe "SetAnalogChannel"
  c_SetAnalogChannel :: CInt -> IO ()

-- | Sets the analogue output channel to maximum
setAnalogChannel :: AnalogOutput -> IO ()
setAnalogChannel channel = do
  c_SetAnalogChannel (analogOutputId channel)


foreign import stdcall unsafe "SetAllAnalog"
  c_SetAllAnalog :: IO ()

-- | Sets all analogue output channels to maximum
setAllAnalog :: IO ()
setAllAnalog = c_SetAllAnalog

