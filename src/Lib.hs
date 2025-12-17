{-# LANGUAGE ViewPatterns #-}
module Lib
    ( someFunc
    ) where
data Client = GovOrg String
    | Company String Integer String String
    | Indivisual String String Bool
    deriving Show
clientName :: Client ->  String
clientName (Company c _ _ _) = c
clientName (GovOrg c) = c
clientName (Indivisual c _ _) = c

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False
someFunc :: IO ()
someFunc = do
    let mycom = Company "Mr. Alejandro" 30 "officer" "Director"
    print $ specialClient mycom
    print $ clientName mycom
    print $ responsibility mycom
    let me = Indivisual "Ben Lim" "student" True
    print $ specialClient me
    print $ clientName me
    print $ responsibility me
