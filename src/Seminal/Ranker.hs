-- | The Ranker allows sorting valid changes, before presenting them to the user
module Seminal.Ranker (sortChanges) where
import Seminal.Change (Change(..))
import Data.List (sortOn)
import Data.Ix (Ix(range))
import Seminal.Compiler.API
import Data.Ord (Down(Down))

-- | Takes the list of successful changes from the searcher.
-- It sorts changes based on their type and index
-- (The deeper <=> the left-most = the better)
sortChanges :: [Change a] -> [Change a]
sortChanges list = snd <$> sortOn cmp (indexed list)
    where
        -- The comparison sorts from best (left-most) to worse (right-most)
        -- (Reverted using `Down`)
        cmp (index, change) = Down (
            -- | The change type is the most important to sort on
            category change,
            realSrcSpan $ location change,
            -- | Then we order by 'when' the change was found
            -- If we found it later, it was deeper in the AST => it's better
            index
            )
        indexed :: [Change a] -> [(Int, Change a)]
        indexed l = zip (range (0, length l)) l