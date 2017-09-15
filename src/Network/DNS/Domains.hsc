{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

module Network.DNS.Domains (
      getDefaultDnsServers
    , isValidIPv4Address
    , newResolvConf
    ) where

import           Foreign.C             (CString, peekCString)
import Foreign.C.String (newCString)
import Foreign.Ptr
import Foreign.Storable (Storable(..))
import           Foreign.Marshal.Utils (maybePeek)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.IP
import qualified Data.Text as T
import Text.Read (readMaybe)
import Network.DNS as DNS

#if !defined(mingw32_HOST_OS)
#define POSIX
#else
#define WIN
#endif

#ifdef WIN
#include "dns.h"
data Dns_t = Dns_t {
    dnsError :: Int
  , dnsAddresses :: String
  } deriving Show

foreign import ccall "getWindowsDefDnsServers" getWindowsDefDnsServers :: IO (Ptr Dns_t)

instance Storable Dns_t where
  alignment _ = #{alignment dns_t}
  sizeOf _    = #{size dns_t}
  peek ptr = do
    a <- #{peek dns_t, error} ptr
    b <- #{peek dns_t, dnsAddresses} ptr >>= peekCString
    return (Dns_t a b)
  poke ptr (Dns_t a b) = do
    #{poke dns_t, error} ptr a
    newCString b >>= #{poke dns_t, dnsAddresses} ptr
#endif

isValidIPv4Address :: String -> Bool
isValidIPv4Address str = case readMaybe @IPv4 str of
  Nothing -> False
  Just _  -> True

-- | Gets one of the default DNS servers the current machine is using.
-- On failure (i.e. when there are no DNS servers available,
-- or when the operation is not supported by the operating system), returns @""@.
--
-- >>> getDefaultDnsServers
-- ["8.8.8.8", "4.4.4.4"]
-- >>> getDefaultDnsServers
-- []
getDefaultDnsServers :: IO [String]
#ifdef WIN
getDefaultDnsServers = do
  res <- peek =<< getWindowsDefDnsServers
  case dnsError res of
    0 -> return $ map T.unpack (T.splitOn "," (T.pack (dnsAddresses res)))
    _ -> return mempty -- TODO: Do proper error handling here.
#else
getDefaultDnsServers = pure ["8.8.8.8", "8.8.4.4"]
#endif

newResolvConf :: IO DNS.ResolvConf
newResolvConf = do
    let googlePublicDNSs = ["8.8.8.8", "8.8.4.4"]
    dns <- (\x -> if x == [] then "8.8.8.8" else head x) <$> getDefaultDnsServers
    return $ DNS.defaultResolvConf { DNS.resolvInfo    = DNS.RCHostName dns
                                   , DNS.resolvTimeout = 10 * 1000 * 1000 -- 10sec timeout
                                   , DNS.resolvRetry   = 10
                                   }
