module Main where

import Network.DNS.Lookup
import Network.DNS.Domains

main :: IO ()
main = getDefaultDnsServers >>= print
