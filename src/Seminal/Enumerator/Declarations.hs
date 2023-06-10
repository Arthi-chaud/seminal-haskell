module Seminal.Enumerator.Declarations(enumerateChangesInDeclaration) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsDecl(..), GhcPs)
import Seminal.Enumerator.Expressions (enumerateChangesInBinding)
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

-- wildcardDecl :: HsDecl GhcPs
-- wildcardDecl = ValD NoExtField wildcardBind
--     where
--         wildcardBind = PatBind EpAnnNotUsed (L noSrcSpanA wildcardPattern) wildcardGRHS ([], [])
--         wildcardPattern= WildPat NoExtField
--         wildcardGRHS = GRHSs emptyComments [L noSrcSpan wildcardGRHS'] (EmptyLocalBinds NoExtField)
--         wildcardGRHS' = GRHS EpAnnNotUsed [] (L noSrcSpanA undefinedExpression)
