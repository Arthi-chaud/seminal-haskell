module Seminal.Enumerator.Literals (enumerateChangeInLiteral) where
import Seminal.Enumerator.Enumerator (Enumerator)
import GHC (HsLit(..), GhcPs)
import Seminal.Change (Change(..), ChangeType (Terminal), node)
import Data.ByteString (unpack)
import Data.ByteString.Internal (w2c)
import GHC.Data.FastString (mkFastString)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Plugins (unpackFS)

-- | Enumeration of changes for Literals, e.g. hard-coded chars, strings, ints etc.
-- See [API doc](https://hackage.haskell.org/package/ghc-9.6.1/docs/Language-Haskell-Syntax-Lit.html#t:HsLit)
enumerateChangeInLiteral :: Enumerator (HsLit GhcPs)
enumerateChangeInLiteral literal loc = case literal of
    (HsChar _ char) -> changeForChar char
    (HsCharPrim _ char) -> changeForChar char
    (HsString _ string) ->
        -- Need to turn FastString to String, for homogeneity
        changeForString $ unpackFS string
    (HsStringPrim _ string) ->
        -- Need to turn ByteString to String, for homogeneity
        changeForString $ w2c <$> unpack string
    _ -> []
    where
        changeForChar char = [
            Change (node literal) [node $ HsString NoSourceText (mkFastString [char])] loc []
            "The expected type of the expression is a String, not a Char. Use double quotes or brackets instead." Terminal -- Turn into String
            ]
        changeForString [char] = [
            Change (node literal) [node $ HsChar NoSourceText char] loc []
            "The expected type of the expression is a Char, not a String. Use single quotes instead." Terminal -- If Singleton, extract it to char
            ]
        changeForString _ = []