module Seminal.Enumerator.Enumerator(Enumerator) where
import Seminal.Change (Change)
import GHC (SrcSpan)
    
-- | Inspired from Seminal (2006, p. 5)
-- The list of changes if sorted by relevancy: the right-most is the most relevant/specific change
type Enumerator a =
    -- | The node of the AST where changes are enumerated
    a ->
    -- | The location of the node (source code-wise).
    SrcSpan ->
    -- | Output: A list of changes for the node 
    [Change a]