
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Network.DNS.Domains

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "getDefaultDnsServer works" testGetDefaultDnsServer
  ]

testGetDefaultDnsServer :: Assertion
testGetDefaultDnsServer = do
  res <- getDefaultDnsServer
  case res of
    Nothing -> fail "getDefaultDnsServer yielded Nothing."
    Just x  -> isValidIPv4Address x @?= True
