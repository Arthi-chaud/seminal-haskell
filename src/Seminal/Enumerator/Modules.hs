module Seminal.Enumerator.Modules(enumerateChangesInModule) where
import Seminal.Compiler.API
import Seminal.Change (Change(Change), ChangeType (Removal), (<&&>), node)
import Seminal.Enumerator.Declarations (enumerateChangesInDeclaration, enumerateChangesInTypeDeclaration)
import Data.List.HT (splitEverywhere)
import Data.Functor ((<&>))
import Seminal.Enumerator.Bindings (enumerateChangesInFuncBinding)

enumerateChangesInModule :: HsModule -> [Change HsModule]
enumerateChangesInModule hsmod = case hsmod of
    HsModule {} -> enumerateChangesAtModuleRoot (hsmodDecls hsmod)
        <&&> (\decls -> hsmod { hsmodDecls = decls })
    -- We do not need to consider extensions
    -- (From 9.6.x)
    -- _ -> []

-- | Enumerate changes for the root declaration of a module
enumerateChangesAtModuleRoot :: [LHsDecl GhcPs] -> [Change [LHsDecl GhcPs]]
enumerateChangesAtModuleRoot list = concat $ splitEverywhere list <&> \(h, L l removed, t) -> let
    (SrcSpanAnn _ removedLoc) = l
    followups = enumerateChangesInDeclaration removed removedLoc <&&> (\r -> h ++ [L l r] ++ t)
    in case removed of
        -- | In the case of a variable, we do not try to remove it, as it may be accompanied by a type declaration,
        -- And standalone type declaration are not allowed
        (ValD v body) -> case body of
            (FunBind {}) -> enumerateChangesInFuncBinding body removedLoc
                <&&> (L l . ValD v)
                <&&> (\change -> h ++ [change] ++ t)
            _ -> []
        (TyClD x typeDecl) -> enumerateChangesInTypeDeclaration typeDecl removedLoc
            <&&> (L l . TyClD x)
            <&&> (\change -> h ++ [change] ++ t)
        _ -> [Change (node list) [node $ h ++ t] removedLoc followups "The removed expression does not type-check." Removal]