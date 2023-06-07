module Seminal.Change (Change(exec, followups, doc), newChange, wrapLoc, ChangeDoc(..), rewriteSrc, ChangeType(..)) where
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
    Maybe String ->
    ChangeType ->
    -- | Output
    Change node
newChange src exec loc followups message category = Change {
    exec = exec,
    followups = followups,
    doc = ChangeDoc {
        location = loc,
        pprSrc = ppr src,
        pprExec = ppr exec,
        message = message,
        category = category
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

instance Eq (Change node) where
    c1 == c2 = doc c1 == doc c2

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
    pprExec :: SDoc,
    -- | An optional message to give to the user
    message :: Maybe String,
    -- | A 'type' of Change, which allows ranking them
    category :: ChangeType
}

instance Eq ChangeDoc where
    d1 == d2 = location d1 == location d2 &&
        showSDocUnsafe (pprSrc d1) == showSDocUnsafe (pprSrc d2) &&
        showSDocUnsafe (pprExec d1) == showSDocUnsafe (pprExec d2) &&
        category d1 == category d2

data ChangeType =
    -- | The Change basically replaces the node with a wildcard.
    -- It is not a conclusive change
    Wildcard |
    -- | A Change that consist in removing a value
    Removal |
    -- | A Change that consist of wrapping the actual value
    -- E.g. `show`
    Wrapping |
    -- | The Change is good enough to terminate the search and/or
    -- be presented to the user as if
    Terminal
    deriving (Eq, Show)

instance Ord ChangeType where
    -- | Ordering Change types by giving each type a number
    -- The higher the number, the better
    compare t1 t2 = compare (n t1) (n t2)
        where
            n :: ChangeType -> Int
            n t = case t of
                Wildcard -> 1
                Removal -> 2
                Wrapping -> 3
                Terminal -> 4


-- | Reset the originally changed node. 
rewriteSrc :: (Outputable node) => node -> Change leaf -> Change leaf
rewriteSrc node change = updatedChange
    where
        updatedChange = change { doc = newdoc }
        newdoc = changedoc { pprSrc = ppr node }
        changedoc = doc change

instance Show ChangeDoc where
    show (ChangeDoc loc src exec _ _) = printf "%s: Replace %s with %s" (showNode loc) (showNode src) (showNode exec)
        where
            showNode :: Outputable a => a -> String
            showNode = showSDocUnsafe . ppr
