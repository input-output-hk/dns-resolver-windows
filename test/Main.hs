{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty
import Data.Monoid
import Test.Tasty.HUnit
import Network.DNS.Domains
import Network.DNS as DNS
import Data.IP

main :: IO ()
main = do
  seed <- newResolvConf >>= makeResolvSeed
  googIP <- withResolver seed $ \resolver -> DNS.lookup resolver "www.google.it" A
  c1     <- withResolver seed $ \resolver -> DNS.lookup resolver "cardano-node-0.aws.iohkdev.io" A
  c2     <- withResolver seed $ \resolver -> DNS.lookup resolver "cardano-node-1.aws.iohkdev.io" A
  c3     <- withResolver seed $ \resolver -> DNS.lookup resolver "reddit.com" A
  putStrLn $ "www.google.it resolved to: " <> show googIP
  putStrLn $ "cardano-node-0.aws.iohkdev.io resolved to: " <> show c1
  putStrLn $ "cardano-node-1.aws.iohkdev.io resolved to: " <> show c2
  putStrLn $ "ntp.pool.org resolved to: " <> show c3

  putStrLn $ "\n*** END OF DEBUG INFO ***\n"
  -- Run the tests
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "multi-value lookups work"  testLookups
  , testCase "resolving IOHK DNS works"  testLookupsIOHK
  ]

testLookups :: Assertion
testLookups = do
  seed <- newResolvConf >>= makeResolvSeed
  res  <- withResolver seed $ \resolver -> DNS.lookup resolver "www.google.it" A
  res2 <- withResolver seed $ \resolver -> DNS.lookup resolver "reddit.com" A
  case res of
    Left e    -> fail (show e)
    Right lst -> assertBool "nay" (all isValid lst)
  case res2 of
    Left e    -> fail (show e)
    Right lst -> assertBool "nay" (all isValid lst && length lst >= 2)

testLookupsIOHK :: Assertion
testLookupsIOHK = do
  seed <- newResolvConf >>= makeResolvSeed
  res  <- withResolver seed $ \resolver -> do
    r1 <- DNS.lookup resolver "cardano-node-0.aws.iohkdev.io" A
    r2 <- DNS.lookup resolver "cardano-node-1.aws.iohkdev.io" A
    return $ r1 >>= (\x -> (x <>) <$> r2)
  case res of
    Left e    -> fail (show e)
    Right lst -> assertBool "nay" (all isValid lst)

isValid :: DNS.RData -> Bool
isValid (RD_A _) = True
isValid x        = error (show x)
