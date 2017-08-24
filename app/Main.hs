module Main where

import Network.DNS.Lookup

main :: IO ()
main = do
  putStrLn "Querying DNS for https://www.google.com ..."
