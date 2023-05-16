module Main (main) where

import GHC.Paths
import Text.Printf
import Lib

main :: IO ()
main = putStrLn $ printf "GHC is installed in %s" libdir
