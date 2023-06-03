module Changes (Change(exec, followups, doc), newChange, wrapLoc, ChangeDoc(..)) where
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

instance Show ChangeDoc where
    show (ChangeDoc loc src exec) = printf "%s: Replace\n%s\n-- with --\n%s" location (showNode src) (showNode exec)
        where
            showNode = showSDocUnsafe . pprLocated . L loc
            location = case loc of
                RealSrcSpan s _ -> printf "Line %d, Col %d" (srcSpanStartLine s) (srcSpanStartCol s)
                UnhelpfulSpan _ -> "[Could not find the location]"

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
--     RemoveItemInList
--     itemToshow 