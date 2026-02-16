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

import Test.Hspec
import Lib

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
