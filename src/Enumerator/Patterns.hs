module Enumerator.Patterns (enumerateChangesInPattern) where
import Enumerator.Enumerator (Enumerator)
import GHC (Pat(..), GhcPs)

-- | Enumerate possible changes for patterns,
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Pat.html#t:Pat)
enumerateChangesInPattern :: Enumerator (Pat GhcPs)
enumerateChangesInPattern _ _ = []