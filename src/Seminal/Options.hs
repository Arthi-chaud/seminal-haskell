module Seminal.Options (Options(..), SearchMethod(..)) where

newtype Options = Options {
    -- | Tells when to stop searching
    search :: SearchMethod
}

data SearchMethod =
    -- | Stop when the bottom of the AST is reached
    Eager |
    -- | Stop when a `Terminal` change is found
    Lazy
    deriving Eq