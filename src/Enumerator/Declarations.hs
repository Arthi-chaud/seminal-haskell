module Enumerator.Declarations(enumerateChangesInDeclaration) where
import Enumerator.Enumerator (Enumerator)
import GHC (HsDecl(..), GhcPs)
import Enumerator.Bindings (enumerateChangesInBinding)
import Changes (wrapChange)
import Data.Functor ((<&>))

-- | Enumerate changes for a declaration.
-- A declaration could be of a type, function, instalce, class etc.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Hs-Decls.html#t:HsDecl)
enumerateChangesInDeclaration :: Enumerator (HsDecl GhcPs)
enumerateChangesInDeclaration (TyClD _ e) loc = []
enumerateChangesInDeclaration (InstD _ _) loc = []
enumerateChangesInDeclaration (DerivD _ _) loc = []
enumerateChangesInDeclaration (ValD i e) loc = enumerateChangesInBinding e loc <&> wrapChange (ValD i)
enumerateChangesInDeclaration (SigD _ e) loc = []
enumerateChangesInDeclaration _ _ = []

-- wildcardDecl :: HsDecl GhcPs
-- wildcardDecl = ValD NoExtField wildcardBind
--     where
--         wildcardBind = PatBind EpAnnNotUsed (L noSrcSpanA wildcardPattern) wildcardGRHS ([], [])
--         wildcardPattern= WildPat NoExtField
--         wildcardGRHS = GRHSs emptyComments [L noSrcSpan wildcardGRHS'] (EmptyLocalBinds NoExtField)
--         wildcardGRHS' = GRHS EpAnnNotUsed [] (L noSrcSpanA undefinedExpression)
