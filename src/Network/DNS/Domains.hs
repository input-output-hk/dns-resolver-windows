{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

module Network.DNS.Domains (
    newResolvConf
    ) where

import Network.DNS as DNS

newResolvConf :: IO DNS.ResolvConf
newResolvConf = do
    -- This will work seamlessly on both Win & Posix.
    return $ DNS.defaultResolvConf { DNS.resolvTimeout = 10 * 1000 * 1000 -- 10sec timeout
                                   , DNS.resolvRetry   = 10
                                   }
