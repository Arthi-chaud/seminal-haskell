module AAAAAAAA where

import Text.Printf

a :: Int
a = 1

main :: IO ()
main = return ()

b = [1, 2, 3]

c = (4, 5, 6)

d = "789"

e = id undefined

type F = [String]

data G = G {
    g :: Int
}

g = G { g = 2 }

h = 'h'