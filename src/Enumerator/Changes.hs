module Enumerator.Changes (Change (..), wrapChange, wrapLoc) where
import GHC (SrcSpan)
import GHC.Plugins
import Text.Printf
import Data.List (intersect, intersperse, intercalate)

-- | Wraps a change of a leaf for/into its parent
wrapChange :: (leaf -> node) -> Change leaf -> Change node
wrapChange f (Change loc exec followups) = Change loc (f exec) (wrapChange f <$> followups)

-- | Rewraps tha location to the change type
wrapLoc :: (SrcSpan -> a -> l) -> Change a -> Change l
wrapLoc f (Change loc exec followups) = Change loc (f loc exec) (wrapLoc f <$> followups)

-- | Defines a change to apply on the AST
data Change node = Change {
    -- | Location (in the source code) of the node to change.
    location :: SrcSpan,
    -- | Run the change, returns the new node
    exec :: node,
    -- | List of subsequent changes to consider, if the parent change succeeds
    followups :: [Change node]
}

instance (Outputable node) => Show (Change node) where
    show (Change loc exec _) = printf "Change at %s: %s" showLoc showExec
        where
            showLoc = showSDocUnsafe (ppr loc)
            showExec = showSDocUnsafe (ppr exec)
            -- showFU = intercalate "\n" $ show <$> followups
-- | Enum of the possible Changes to apply on the AST
-- Inspired by the list provided in the Seminal paper (2006, p. 4)
-- data ChangeType =
--     -- | Replace an argument in application with `undefined`
--     GenericReplacement Int |
--     -- | Add arbitrary new argument to function call
--     AddArg |
--     -- | Delete argument to function call
--     DelArg |
--     -- | Swap arguments to function call
--     SwapArg |
--     -- | Turns an array into a tuple
--     ArrayToTuple |
--     -- | Turns a tuple into an array
--     TupleToArray |
--     -- | Turns application arguments into a tuple
--     -- | Either all of them, or some
--     ArgsToTuple |
--     -- | Remove `case of` match
--     RemoveCaseMatch |
--     -- | Set value of `case of` match to undefined
--     SetCaseToUndef |
--     -- | Set match of `case of` match to `_`
--     SetMatchToWildcard |
--     -- | Remove type signature
--     RemoveTypeSignature |
--     -- | Replace `x` in `if x then ... else ...` by `True`
--     IfToTrue |
--     -- | Set value of `let x = ` to `undefined`
--     LetToUndef |
--     -- | Set one value in `where` clause to `undefined`
--     WhereToUndef |
--     -- | Turn `[x]` into `x`
--     SingletonToItem |
--     -- | Turn `x` into `[x]`
--     ItemToSingleton |
--     CharToString |
--     StringToChar |
--     -- | Remove declaration (using its index) from list
--     RemoveDeclaration Int
