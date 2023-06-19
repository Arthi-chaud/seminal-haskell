{-# LANGUAGE DeriveDataTypeable #-}
module Seminal.Change (Change(..), node, getNode, ChangeNode(pretty), (<$$>), (<&&>), Seminal.Change.show, ChangeType(..), changeTypes) where

import GHC (SrcSpan)
import GHC.Plugins (SDoc, Outputable, ppr, showSDocUnsafe)
import Text.Printf (printf)
import Data.Data (dataTypeConstrs, Data (dataTypeOf), showConstr)
type ChangeLocation = SrcSpan

-- | Sub-data, that could be either the original node, or the change 
data ChangeNode nodeType = ChangeNode {
    -- | The AST node
    astNode :: nodeType,
    -- | Pretty-print of the node
    pretty :: SDoc
}

instance Functor ChangeNode where
    fmap f n = n { astNode = f (astNode n) }

-- | Builds `ChangeNode` from an AST node
node :: (Outputable n) => n -> ChangeNode n
node n = ChangeNode n (ppr n)

getNode :: ChangeNode n -> n
getNode = astNode

-- | Defines a change to apply on the AST.
-- The namings are inspired by the `astRepl` (Seminal, 2006, p. 5)
data Change node = Change {
    src :: ChangeNode node,
    -- | Run the change, returns the new node
    exec :: [ChangeNode node],
    location :: ChangeLocation,
    -- | List of subsequent changes to consider, if the `change` typechecks
    followups :: [Change node],
    message :: Maybe String,
    category :: ChangeType
}

instance Functor Change where
    fmap f c = case c of
        Change {} -> c {
            src = f <$> src c,
            exec = fmap f <$> exec c,
            followups = fmap f <$> followups c
        }

(<$$>) :: (a -> b) -> [Change a]  -> [Change b]
f <$$> list = fmap f <$> list 

(<&&>) :: [Change a] -> (a -> b) -> [Change b]
(<&&>) = flip (<$$>)

show :: ChangeNode node -> ChangeNode node -> ChangeLocation -> Maybe String -> String
show src_ exec_ loc message_  = replacement ++ case message_ of
        (Just msg) -> '\n' : msg
        _ -> ""
        where
            replacement = printf "%s: Replace `%s` with `%s`"
                (showSDocUnsafe $ ppr loc)
                (showSDocUnsafe $ pretty src_)
                (showSDocUnsafe $ pretty exec_)

-- | Categories of changes, that allow ordering them
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
    deriving (Eq, Show, Read, Data)

changeTypes :: [String]
changeTypes = showConstr <$> dataTypeConstrs  (dataTypeOf Terminal)

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