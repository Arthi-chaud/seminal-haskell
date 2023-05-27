module Main (main) where

import GHC.Paths
import Text.Printf

main :: IO ()
main = putStrLn $ printf "GHC is installed in %s" libdir

