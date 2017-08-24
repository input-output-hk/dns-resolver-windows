{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Network.DNS.Domains (
      getDefaultDnsServer
    , isValidIPv4Address
    ) where

import           Foreign.C             (CString, peekCString)
import           Foreign.Marshal.Utils (maybePeek)
import Data.IP
import Text.Read (readMaybe)
import Network.DNS as DNS

#if !defined(mingw32_HOST_OS)
#define POSIX
#else
#define WIN
#endif



#ifdef WIN
-- | Returns @nullPtr@ on failure.
foreign import ccall "getWindowsDefDnsServer" getWindowsDefDnsServer :: IO CString
#endif

isValidIPv4Address :: String -> Bool
isValidIPv4Address str = case readMaybe @IPv4 str of
  Nothing -> False
  Just _  -> True

-- | Gets one of the default DNS servers the current machine is using.
-- On failure (i.e. when there are no DNS servers available,
-- or when the operation is not supported by the operating system), returns @""@.
--
-- >>> getDefaultDnsServer
-- Just "8.8.8.8"
-- >>> getDefaultDnsServer
-- Nothing
getDefaultDnsServer :: IO (Maybe String)
#ifdef WIN
getDefaultDnsServer = getWindowsDefDnsServer >>= maybePeek peekCString
#else
getDefaultDnsServer = pure (Just "8.8.8.8")
#endif

newResolvConf :: IO DNS.ResolvConf
newResolvConf =
#ifdef POSIX
    return DNS.defaultResolvConf
#else
    let googlePublicDNS = "8.8.8.8"
    dns <- fromMaybe googlePublicDNS <$> getDefaultDnsServer
    return $ DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName dns }
#endif
