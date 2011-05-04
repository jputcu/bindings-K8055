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
  module Bindings.K8055.AnalogIn,
  module Bindings.K8055.AnalogOut,
  module Bindings.K8055.DigitalOut,
  module Bindings.K8055.DigitalIn,
  module Bindings.K8055.Counters
  ) where

import Foreign.C
import Control.Monad
import Bindings.K8055.AnalogIn
import Bindings.K8055.AnalogOut
import Bindings.K8055.DigitalOut
import Bindings.K8055.DigitalIn
import Bindings.K8055.Counters


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
