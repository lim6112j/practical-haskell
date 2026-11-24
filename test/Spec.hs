{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Test suite for the application.
Copyright   : (c) 2026, Your Name
License     : MIT
Maintainer  : your.email@example.com
Stability   : experimental

This module contains the test suite for the application.
-}

module Main (main) where

import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ex12UrlShortener
  ( base62Decode, base62Encode, countByDay, genCode, normalizeUrl )
import Lib (Client (..), clientName, responsibility, specialClient)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Lib" $ do
        it "should correctly identify special clients" $ do
            let client1 = Company "Mr. Alejandro" 30 "officer" "Director"
            let client2 = Individual "Ben Lim" "student" True
            specialClient client1 `shouldBe` True
            specialClient client2 `shouldBe` False

        it "should correctly extract the name of a client" $ do
            let client1 = Company "Mr. Alejandro" 30 "officer" "Director"
            let client2 = Individual "Ben Lim" "student" True
            clientName client1 `shouldBe` "Mr. Alejandro"
            clientName client2 `shouldBe` "Ben Lim"

        it "should correctly determine the responsibility of a client" $ do
            let client1 = Company "Mr. Alejandro" 30 "officer" "Director"
            let client2 = Individual "Ben Lim" "student" True
            responsibility client1 `shouldBe` "Director"
            responsibility client2 `shouldBe` "Unknown"

    -- Ex12 의 순수 코어만 검증한다 (DB/서버는 테스트하지 않는다).
    -- TODO 1~6 을 채우기 전까지는 이 부분이 실패한다.
    describe "Ex12 순수 코어 (TODO 1~6)" $ do
      it "base62 인코딩" $ do
        base62Encode 0 `shouldBe` "0"
        base62Encode 61 `shouldBe` "Z"
        base62Encode 62 `shouldBe` "10"

      it "base62 왕복" $ do
        mapM_
          ( \n -> base62Decode (base62Encode n) `shouldBe` Just n
          )
          [0, 1, 61, 62, 1000, 281474976710656]

      it "normalizeUrl" $ do
        normalizeUrl "haskell.org" `shouldBe` Right "https://haskell.org"
        normalizeUrl "  HTTP://X.COM/A " `shouldBe` Right "http://x.com/A"
        normalizeUrl "ftp://nope" `shouldSatisfy` either (const True) (const False)
        normalizeUrl "" `shouldSatisfy` either (const True) (const False)

      it "genCode 는 8 자리" $ do
        T.length (genCode 42) `shouldBe` 8

      it "countByDay" $ do
        let t n = posixSecondsToUTCTime (fromInteger n)
        countByDay [t 100000, t 100100, t 200000]
          `shouldBe` [("1970-01-02", 2), ("1970-01-03", 1)]
        countByDay [] `shouldBe` []
