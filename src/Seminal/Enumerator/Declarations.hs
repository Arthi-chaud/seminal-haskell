module Seminal.Enumerator.Declarations(enumerateChangesInDeclaration) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsDecl(..), GhcPs)
import Seminal.Enumerator.Bindings (enumerateChangesInBinding)
import Seminal.Change ((<&&>))

-- | Enumerate changes for a declaration.
-- A declaration could be of a type, function, instalce, class etc.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Hs-Decls.html#t:HsDecl)
enumerateChangesInDeclaration :: Enumerator (HsDecl GhcPs)
enumerateChangesInDeclaration (TyClD _ _) _ = []
enumerateChangesInDeclaration (InstD _ _) _ = []
enumerateChangesInDeclaration (DerivD _ _) _ = []
enumerateChangesInDeclaration (ValD i e) loc = enumerateChangesInBinding e loc <&&> (ValD i)
enumerateChangesInDeclaration (SigD _ _) _ = []
enumerateChangesInDeclaration _ _ = []
