module Enumerator.Enumerator(Enumerator) where
import Changes (Change)
import GHC (SrcSpan)
    
-- | Inspired from Seminal (2006, p. 5)
type Enumerator a =
    -- | The node of the AST where changes are enumerated
    a ->
    -- | The location of the node (source code-wise).
    SrcSpan ->
    -- | Output: A list of changes for the node 
    [Change a]