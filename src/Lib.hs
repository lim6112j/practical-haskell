{-|
Module      : Lib
Description : Utility functions and data types.
Copyright   : (c) 2026, Your Name
License     : MIT
Maintainer  : your.email@example.com
Stability   : experimental

This module provides utility functions and data types used throughout the application.
-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , main
    , Client(..)
    , clientName
    , responsibility
    , specialClient
    , ClientR(..)
    , PersonR(..)
    ) where

import Control.Lens (makeLenses, (^.), (.~), set)

{-|
Data type representing a client, which can be either a Government Organization,
a Company, or an Individual.
-}
data Client = GovOrg String
    | Company String Integer String String
    | Individual String String Bool
    deriving Show

{-|
Extracts the name of a client from the `Client` data type.
-}
clientName :: Client ->  String
clientName (Company c _ _ _) = c
clientName (GovOrg c) = c
clientName (Individual c _ _) = c

{-|
Determines the responsibility of a client if applicable. Returns "Unknown" otherwise.
-}
responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

{-|
Checks if a client is special based on specific criteria.
-}
specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

{-|
Data type representing a client with record syntax, which can be either a Government Organization,
a Company, or an Individual.
-}
data ClientR = GovOrgR { _clientRName :: String }
    | CompanyR { _clientRName :: String
              , _companyId:: Integer
              , _person :: PersonR
              , _duty :: String }
    | IndividualR { _person :: PersonR }
    deriving Show

{-|
Data type representing a person with record syntax.
-}
data PersonR = PersonR { _firstName :: String
                      , _lastName :: String
                      } deriving (Show, Eq, Read)

makeLenses ''ClientR
makeLenses ''PersonR

{-|
Example function that demonstrates some functionality of the module.
-}
someFunc :: IO ()
someFunc = do
    let mycom = Company "Mr. Alejandro" 30 "officer" "Director"
    print $ specialClient mycom
    print $ clientName mycom
    print $ responsibility mycom
    let me = Individual "Ben Lim" "student" True
    print $ specialClient me
    print $ clientName me
    print $ responsibility me
    let p = read  "PersonR {_firstName = \"Ben\", _lastName = \"Lim\"}" :: PersonR 
    print $ p ^. firstName
    let p2 = set firstName "John" p
    print $ p2 ^. firstName

main :: IO ()
main = someFunc
