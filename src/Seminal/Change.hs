{-# LANGUAGE DeriveDataTypeable #-}
module Seminal.Change (Change(..), node, getNode, ChangeNode(pretty), (<$$>), (<&&>), Seminal.Change.show, Seminal.Change.showWithMessage, ChangeType(..), changeTypes, forceRewrite) where

import Seminal.Compiler.API
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
    -- | A User-friendly message to explain why the change worked
    message :: String,
    -- | Allows ranking the changes.
    category :: ChangeType
}

instance Functor Change where
    fmap f c = c {
        src = f <$> src c,
        exec = fmap f <$> exec c,
        followups = fmap f <$> followups c
    }

forceRewrite :: Outputable node => Change node -> Change node
forceRewrite change = change { 
    src = node . getNode $ src change,
    exec = node . getNode <$> exec change,
    followups = forceRewrite <$> followups change
} 

(<$$>) :: (a -> b) -> [Change a]  -> [Change b]
f <$$> list = fmap f <$> list

(<&&>) :: [Change a] -> (a -> b) -> [Change b]
(<&&>) = flip (<$$>)

show :: ChangeNode node -> ChangeNode node -> ChangeLocation -> String
show src_ exec_ loc  = printf "%s:\nReplace\t`%s`\nwith\t`%s`"
    (showPprUnsafe loc)
    (showPprUnsafe $ pretty src_)
    (showPprUnsafe $ pretty exec_)

showWithMessage :: ChangeNode node -> ChangeNode node -> ChangeLocation -> String -> String
showWithMessage src_ exec_ loc message_  = Seminal.Change.show src_ exec_ loc ++ "\nReason: " ++ message_

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
    Terminal |
    -- We add *something* in the AST to make things work.
    -- This addition is usually a wildcard, making the change not very usefull
    Addition
    deriving (Eq, Show, Read, Data)

changeTypes :: [String]
changeTypes = showConstr <$> dataTypeConstrs (dataTypeOf Terminal)

instance Ord ChangeType where
    -- | Ordering Change types by giving each type a number
    -- The higher the number, the better
    compare t1 t2 = compare (n t1) (n t2)
        where
            n :: ChangeType -> Int
            n t = case t of
                Wildcard -> 1
                Addition -> 2
                Removal -> 3
                Wrapping -> 4
                Terminal -> 5