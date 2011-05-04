module Bindings.K8055.DigitalOut (
  DigitalOutput(..),
  writeAllDigital,
  clearDigitalChannel,
  clearAllDigital,
  setDigitalChannel,
  setAllDigital
  ) where

import Foreign.C
import Data.Word

data DigitalOutput
  = DigitalOut1
  | DigitalOut2
  | DigitalOut3
  | DigitalOut4
  | DigitalOut5
  | DigitalOut6
  | DigitalOut7
  | DigitalOut8

digitalOutputId :: Num a => DigitalOutput -> a
digitalOutputId output =
  case output of
    DigitalOut1 -> 1
    DigitalOut2 -> 2
    DigitalOut3 -> 3
    DigitalOut4 -> 4
    DigitalOut5 -> 5
    DigitalOut6 -> 6
    DigitalOut7 -> 7
    DigitalOut8 -> 8


foreign import stdcall unsafe "WriteAllDigital"
  c_WriteAllDigital :: CInt -> IO ()

-- | Sets the digital outputs according to the data
writeAllDigital :: Word8 -> IO ()
writeAllDigital val =
  c_WriteAllDigital (fromIntegral val)


foreign import stdcall unsafe "ClearDigitalChannel"
  c_ClearDigitalChannel :: CInt -> IO ()

-- | Clears the output channel
clearDigitalChannel :: DigitalOutput -> IO ()
clearDigitalChannel dig =
  c_ClearDigitalChannel (digitalOutputId dig)


foreign import stdcall unsafe "ClearAllDigital"
  c_ClearAllDigital :: IO ()

-- | Clears all output channels
clearAllDigital :: IO ()
clearAllDigital = c_ClearAllDigital


foreign import stdcall unsafe "SetDigitalChannel"
  c_SetDigitalChannel :: CInt -> IO ()

-- | Sets the output channel
setDigitalChannel :: DigitalOutput -> IO ()
setDigitalChannel dig = 
  c_SetDigitalChannel (digitalOutputId dig)


foreign import stdcall unsafe "SetAllDigital"
  c_SetAllDigital :: IO ()

-- | Sets all output channels
setAllDigital :: IO ()
setAllDigital = c_SetAllDigital



