module Seminal.Enumerator.Patterns (enumerateChangesInPattern) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (Pat(..), GhcPs)
import Seminal.Enumerator.Literals (enumerateChangeInLiteral)
import Data.Functor ((<&>))

-- | Enumerate possible changes for patterns,
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Pat.html#t:Pat)
enumerateChangesInPattern :: Enumerator (Pat GhcPs)
enumerateChangesInPattern pat loc = case pat of
    (LitPat xlit litExpr) -> enumerateChangeInLiteral litExpr loc <&> fmap (LitPat xlit)
    _ -> []