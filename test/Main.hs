{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Network.DNS.Domains
import Network.DNS as DNS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "getDefaultDnsServer works" testGetDefaultDnsServer
  , testCase "multi-value lookups work"  testLookups
  ]

testGetDefaultDnsServer :: Assertion
testGetDefaultDnsServer = do
  res <- getDefaultDnsServer
  case res of
    Nothing -> fail "getDefaultDnsServer yielded Nothing."
    Just x  -> isValidIPv4Address x @?= True

testLookups :: Assertion
testLookups = do
  seed <- newResolvConf >>= makeResolvSeed
  res  <- withResolver seed $ \resolver -> DNS.lookup resolver "www.google.it" A
  case res of
    Left e    -> fail (show e)
    Right lst -> assertBool "nay" (all (isValidIPv4Address . show) lst)
