{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module LookupSpec where

import Network.DNS as DNS
import Test.Hspec

#if !defined(mingw32_HOST_OS)
#define POSIX
#else
#define WIN
#endif

resolvConf :: DNS.ResolvConf
#ifdef WIN
resolvConf =
  let googlePublicDNS = "8.8.8.8"
  in DNS.defaultResolvConf { DNS.resolvInfo    = DNS.RCHostName googlePublicDNS
                           }
#else
resolvConf = defaultResolvConf
#endif

spec :: Spec
spec = describe "lookup" $ do

    it "lookupA" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            addrs <- DNS.lookupA resolver "mew.org"
            -- mew.org has one or more IPv6 addresses
            fmap null addrs `shouldBe` Right False

    it "lookupAAAA" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            -- google.com has one or more IPv6 addresses
            addrs <- DNS.lookupAAAA resolver "google.com"
            fmap null addrs `shouldBe` Right False

    it "lookupAAAA with emty result" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            addrs <- DNS.lookupAAAA resolver "mew.org"
            -- mew.org does not have any IPv6 addresses
            fmap null addrs `shouldBe` Right True

    it "lookupMX" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            addrs <- DNS.lookupMX resolver "mew.org"
            -- mew.org has one or more MX records.
            fmap null addrs `shouldBe` Right False

    it "lookupTXT" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            addrs <- DNS.lookupTXT resolver "mew.org"
            -- mew.org has one or more TXT records.
            fmap null addrs `shouldBe` Right False

    it "lookupNS" $ do
        rs <- makeResolvSeed resolvConf
        withResolver rs $ \resolver -> do
            addrs <- DNS.lookupNS resolver "mew.org"
            -- mew.org has one or more NS records.
            fmap null addrs `shouldBe` Right False
