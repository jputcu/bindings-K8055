{-
 * Create K8055D.def with exports
 * dlltool -d K8055D.def -l K8055D.a
 * Use --extra-lib-dirs=. with cabal configure/install
-}
module Bindings.K8055 (
  getVersion,
  -- * General Procedures
  CardAddress(..),
  withDevice,
  -- * Analogue to Digital converter procedures
  AnalogInput(..),
  readAnalogChannel, 
  readAllAnalog,
  -- * Digital to Analogue conversion procedures
  AnalogOutput(..),
  outputAnalogChannel,
  outputAllAnalog,
  clearAnalogChannel,
  clearAllAnalog,
  setAnalogChannel,
  setAllAnalog,
  -- * Digital Output procedures 
  DigitalOutput(..),
  writeAllDigital,
  clearDigitalChannel,
  clearAllDigital,
  setDigitalChannel,
  setAllDigital,
  -- * Digital Input procedures and functions
  DigitalInput(..),
  readDigitalChannel,
  readAllDigital,
  -- * Counter procedures and functions
  Counter(..),
  resetCounter,
  readCounter,
  setCounterDebounceTime
  ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad
import Data.Word


-- | Depends on jumpers SK5, SK6 
data CardAddress 
  = Card1  -- ^ SK5:ON  SK6:ON
  | Card2  -- ^ SK5:OFF SK6:ON
  | Card3  -- ^ SK5:ON  SK6:OFF
  | Card4  -- ^ SK5:OFF SK6:OFF
    
addressId :: Num a => CardAddress -> a
addressId address = 
  case address of
    Card1 -> 0
    Card2 -> 1
    Card3 -> 2
    Card4 -> 3

    
data AnalogInput
  = AnalogIn1
  | AnalogIn2
    
analogInputId :: Num a => AnalogInput -> a
analogInputId input =
  case input of
    AnalogIn1 -> 1
    AnalogIn2 -> 2
   
    
data AnalogOutput
  = AnalogOut1
  | AnalogOut2
    
analogOutputId :: Num a => AnalogOutput -> a
analogOutputId output =                                       
  case output of
    AnalogOut1 -> 0
    AnalogOut2 -> 1


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


data Counter
  = Counter1
  | Counter2
    
counterId :: Num a => Counter -> a
counterId cnt =
  case cnt of
    Counter1 -> 1
    Counter2 -> 2


foreign import stdcall unsafe "Version"
  c_Version :: IO CInt
               
getVersion :: IO Int
getVersion = do
  vers <- c_Version
  return $ fromIntegral vers


foreign import stdcall unsafe "OpenDevice"
  c_OpenDevice :: CInt -> IO CInt

foreign import stdcall unsafe "CloseDevice"
  c_CloseDevice :: IO ()
                   
-- | Device is opened, action is performed and device is closed
withDevice :: CardAddress -> IO a -> IO a 
withDevice address action = do
  ret <- c_OpenDevice (addressId address)
  when (ret /= 0) (error "open device failed")
  result <- action
  c_CloseDevice
  return result


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
setCounterDebounceTime :: Counter   -- ^ 0 - 5000 
                          -> Int 
                          -> IO ()
setCounterDebounceTime cnt debounce =
  c_SetCounterDebounceTime (counterId cnt) (fromIntegral debounce)


