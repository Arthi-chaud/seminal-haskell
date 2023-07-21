module Seminal.Enumerator.Declarations(enumerateChangesInDeclaration, enumerateChangesInTypeDeclaration) where
import Seminal.Enumerator.Enumerator (Enumerator)
import Seminal.Compiler.API
import Seminal.Enumerator.Bindings (enumerateChangesInBinding)
import Seminal.Change ((<&&>), forceRewrite)
import Seminal.Enumerator.Signatures (enumerateChangeInSignature)
import Seminal.Enumerator.Types (enumerateChangeInType)
import Data.Functor ((<&>))

-- | Enumerate changes for a declaration.
-- A declaration could be of a type, function, instalce, class etc.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/GHC-Hs-Decls.html#t:HsDecl)
enumerateChangesInDeclaration :: Enumerator (HsDecl GhcPs)
enumerateChangesInDeclaration (TyClD x typDecl) loc = enumerateChangesInTypeDeclaration typDecl loc
    <&&> (TyClD x)
enumerateChangesInDeclaration (InstD _ _) _ = []
enumerateChangesInDeclaration (DerivD _ _) _ = []
enumerateChangesInDeclaration (ValD i e) loc = enumerateChangesInBinding e loc <&&> (ValD i)
enumerateChangesInDeclaration (SigD x sig) loc = enumerateChangeInSignature sig loc <&&> (SigD x)
enumerateChangesInDeclaration _ _ = []

enumerateChangesInTypeDeclaration :: Enumerator (TyClDecl GhcPs)
enumerateChangesInTypeDeclaration decl _ = case decl of
    (SynDecl x name var fixity (L l typeValue)) -> enumerateChangeInType typeValue (locA l)
        <&&> (SynDecl x name var fixity . L l)
        <&> forceRewrite
    _ -> []