module Seminal.Options (Options(..), SearchMethod(..)) where

data Options = Options {
    -- | Tells when to stop searching
    search :: SearchMethod,
    -- | If True, for each call to the typechecker, will print infor about it
    traceTcCalls :: Bool
}

data SearchMethod =
    -- | Stop when the bottom of the AST is reached
    Eager |
    -- | Stop when a `Terminal` change is found
    Lazy
    deriving Eq