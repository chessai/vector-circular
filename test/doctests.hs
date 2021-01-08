module Main (main) where

import Test.DocTest


main :: IO ()
main = doctest $ ["-isrc" ] ++ ghcExts ++ files


ghcExts :: [String]
ghcExts = map ("-X" ++)
          [ "Haskell2010"
          , "TypeApplications"
          ]

files :: [String]
files = map toFile modules


toFile :: String -> String
toFile = (\s -> "src/" <> s <> ".hs") . replace '.' '/'

replace     :: Eq a => a -> a -> [a] -> [a]
replace a b = go
  where
    go []                 = []
    go (c:cs) | c == a    = b:go cs
              | otherwise = c:go cs

modules :: [String]
modules =
  [ "Data.Vector.Circular"
  , "Data.Vector.Circular.Generic"
  ]
