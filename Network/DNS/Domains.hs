{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Pos.Network.Windows.DnsDomains (
      getDefaultDnsServer
    , isValidIPv4Address
    ) where

import           Foreign.C             (CString, peekCString)
import           Foreign.Marshal.Utils (maybePeek)
import Data.IP
import Text.Read (readMaybe)


#if mingw32_HOST_OS == 1
-- | Returns @nullPtr@ on failure.
foreign import ccall "getWindowsDefDnsServer" getWindowsDefDnsServer :: IO CString
#endif

isValidIPv4Address :: String -> Bool
isValidIPv4Address str = case readMaybe @IPv4 str of
  Nothing -> False
  Just _  -> True

-- | Gets one of the default DNS servers the current machine is using. To be used
-- on Windows systems only. On failure (i.e. when there are no DNS servers available,
-- or when the operation is not supported by the operating system), returns @""@.
--
-- >>> getWindowsDefaultDnsServer
-- Just "8.8.8.8"
-- >>> getWindowsDefaultDnsServer
-- Nothing
getDefaultDnsServer :: IO (Maybe String)
#if mingw32_HOST_OS == 1
getDefaultDnsServer = getWindowsDefDnsServer >>= maybePeek peekCString
#else
getDefaultDnsServer = pure Nothing
#endif
