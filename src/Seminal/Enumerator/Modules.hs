module Seminal.Enumerator.Modules(enumerateChangesInModule) where
import GHC (LHsDecl, GhcPs, SrcSpanAnn' (..), HsDecl (ValD), HsBindLR (FunBind), GenLocated (L), HsModule (HsModule, hsmodDecls))
import Seminal.Change (Change, newChange, wrapLoc, ChangeType (Removal), (<&&>))
import Seminal.Enumerator.Declarations (enumerateChangesInDeclaration)
import Data.List.HT (splitEverywhere)
import Data.Functor ((<&>))
import Seminal.Enumerator.Expressions (enumerateChangesInFuncBinding)

enumerateChangesInModule :: HsModule -> [Change HsModule]
enumerateChangesInModule hsmod = case hsmod of
    HsModule {} -> enumerateChangesAtModuleRoot (hsmodDecls hsmod)
        <&&> (\decls -> hsmod { hsmodDecls = decls })
    -- We do not need to consider extensions
    -- (From 9.6.x)
    -- _ -> []

-- | Enumerate changes for the root declaration of a module
enumerateChangesAtModuleRoot :: [LHsDecl GhcPs] -> [Change [LHsDecl GhcPs]]
enumerateChangesAtModuleRoot list = concat $ splitEverywhere list <&> (\(h, L l removed, t) -> let
    (SrcSpanAnn ep removedLoc) = l
    followups = (enumerateChangesInDeclaration removed removedLoc
        <&> wrapLoc (L . SrcSpanAnn ep))
        <&&> (\r -> h ++ [r] ++ t)
    in case removed of
        -- | In the case of a variable, we do not try to remove it, as it may be accompanied by a type declaration,
        -- And standalone type declaration are not allowed
        (ValD v (FunBind a b c d)) -> enumerateChangesInFuncBinding (FunBind a b c d) removedLoc
            <&&> (L l . ValD v)
            <&&> (\change -> h ++ [change] ++ t)
        _ -> [newChange list (h ++ t) removedLoc followups Nothing Removal]
    )