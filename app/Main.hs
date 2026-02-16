{-|
Module      : Main
Description : Entry point of the application.
Copyright   : (c) 2026, Your Name
License     : MIT
Maintainer  : your.email@example.com
Stability   : experimental

This module contains the main entry point for the application.
-}

module Main (main) where

import Lib
import LazynessTimemachine

{-|
The main function is the entry point of the application. It currently calls `timeMachineFunc`.
-}
main :: IO ()
--main = someFunc
-- main = complexFunc
-- main = treeFunc
-- main = foldFunc
main = timeMachineFunc
