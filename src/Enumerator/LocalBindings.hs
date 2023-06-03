module Enumerator.LocalBindings(enumerateChangesInLocalBinds) where
import GHC (HsLocalBindsLR(..), GhcPs)
import Enumerator.Enumerator (Enumerator)
import GHC.Hs (HsLocalBinds)

-- | Enumeration of changes for local bindings, e.g. in a `let` or `where` clause
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Binds.html#t:HsLocalBindsLR)
enumerateChangesInLocalBinds :: Enumerator (HsLocalBinds GhcPs)
enumerateChangesInLocalBinds (HsValBinds _ _) l = []
enumerateChangesInLocalBinds (HsIPBinds _ _) l = []
-- The other cases (`EmptyLocalBinds`, and extensions) do not need to be considered 
enumerateChangesInLocalBinds _ _ = []