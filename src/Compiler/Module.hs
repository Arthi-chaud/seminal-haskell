module Compiler.Module (
    applyAST
) where
import GHC (HsModule, ParsedModule (ParsedModule), getLoc, GenLocated(L))

-- | Apply (possibly modified) HsModule to ParsedModule,
-- | To allow sending it to the typechecker 
applyAST :: HsModule -> ParsedModule -> ParsedModule
applyAST ast (ParsedModule summary src file) = ParsedModule summary (L loc ast) file
    where
        loc = getLoc src