module Changes (Change(exec, followups, doc), newChange, wrapLoc, ChangeDoc(..), rewriteSrc) where
import GHC.Plugins
import Text.Printf

-- | Exported constructor for `Change`.
-- It will allow easier build of related doc
newChange :: (Outputable node) =>
    -- | The node that is changed
    node ->
    -- | The new node
    node ->
    -- | The location of the original change
    SrcSpan ->
    -- | Followups
    [Change node] ->
    -- | Output
    Change node
newChange src exec loc followups = Change {
    exec = exec,
    followups = followups,
    doc = ChangeDoc {
        location = loc,
        pprSrc = ppr src,
        pprExec = ppr exec
    }
}

-- | Rewraps tha location to the change type
wrapLoc :: (SrcSpan -> a -> l) -> Change a -> Change l
wrapLoc f change = change {
    exec = f (location $ doc change) $ exec change,
    followups = wrapLoc f <$> followups change
}

-- | Defines a change to apply on the AST.
-- The namings are inspired by the `astRepl` (Seminal, 2006, p. 5)
data Change node = Change {
    -- | Run the change, returns the new node
    exec :: node,
    -- | List of subsequent changes to consider, if the parent change succeeds
    followups :: [Change node],
    -- | Pretty-printable Information about the change
    doc :: ChangeDoc
}

instance Show (Change node) where
    show change = show $ doc change

instance Functor Change where
    fmap f change = change {
        exec = f $ exec change,
        followups = fmap f <$> followups change
    }

-- | Gives information about a change
data ChangeDoc = ChangeDoc {
    -- | Location (in the source code) of the changed node.
    location :: SrcSpan,
    -- | The PrettyPrint of the changed subnode
    pprSrc :: SDoc,
     -- | The PrettyPrint of the new subnode
    pprExec :: SDoc
}

-- | Reset the originally changed node. 
rewriteSrc :: (Outputable node) => node -> Change leaf -> Change leaf
rewriteSrc node change = updatedChange 
    where
        updatedChange = change { doc = newdoc }
        newdoc = changedoc { pprSrc = ppr node }
        changedoc = doc change

instance Show ChangeDoc where
    show (ChangeDoc loc src exec) = printf "%s: Replace %s with %s" (showNode loc) (showNode src) (showNode exec)
        where
            showNode :: Outputable a => a -> String
            showNode = showSDocUnsafe . ppr

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
--     -- | Turns application arguments into a tuple
--     -- | Either all of them, or some
--     ArgsToTuple |
--     -- | Remove `case of` match
--     RemoveCaseMatch |
--     -- | Set value of `case of` match to undefined
--     SetCaseToUndef |
--     -- | Set match of `case of` match to `_`
--     SetMatchToWildcard |
--     -- | Replace `x` in `if x then ... else ...` by `True`
--     IfToTrue |
--     -- | Set value of `let x = ` to `undefined`
--     LetToUndef |
--     -- | Set one value in `where` clause to `undefined`
--     WhereToUndef |
--     -- | Remove declaration
--     RemoveDeclaration