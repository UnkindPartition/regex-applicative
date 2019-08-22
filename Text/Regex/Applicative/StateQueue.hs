-- | This internal module is exposed only for testing and benchmarking. You
-- don't need to import it.
{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Applicative.StateQueue
    ( StateQueue
    , empty
    , insert
    , insertUnique
    , getElements
    ) where

import Prelude hiding (read, lookup, replicate)
import qualified Data.IntSet as IntSet
import Data.Foldable as F

-- | 'StateQueue' is a data structure that can efficiently insert elements
-- (preserving their order)
-- and check whether an element with the given 'Int' key is already in the queue.
data StateQueue a = StateQueue
    { elements :: [a]
    , ids :: !IntSet.IntSet
    }
    deriving (Eq,Show)

instance Foldable StateQueue where
  foldr f a = F.foldr f a . getElements

-- | Get the list of all elements
getElements :: StateQueue a -> [a]
getElements = reverse . elements

{-# INLINE empty #-}
-- | The empty state queue
empty :: StateQueue a
empty = StateQueue
    { elements = []
    , ids = IntSet.empty
    }

{-# INLINE insert #-}
-- | Insert an element in the state queue, unless there is already an element with the same key
insertUnique
    :: Int -- ^ key
    -> a
    -> StateQueue a
    -> StateQueue a
insertUnique i v sq@StateQueue { ids = ids, elements = elements } =
    if i `IntSet.member` ids
        then sq
        else sq { elements = v : elements
                , ids = IntSet.insert i ids
                }

-- | Insert an element in the state queue without a key.
--
-- Since 'insert' doesn't take a key, it won't affect any 'insertUnique'.
insert
    :: a
    -> StateQueue a
    -> StateQueue a
insert v sq =
    sq { elements = v : elements sq }
