module Text.Regex.Applicative.StateQueue
    ( StateQueue
    , empty
    , insert
    , insertUnique
    , fold
    , getElements
    ) where

import Prelude hiding (read, lookup, replicate)
import qualified Data.IntSet as IntSet
import Data.List (foldl')

data StateQueue a = StateQueue
    { elements :: [a]
    , ids :: !IntSet.IntSet
    }

getElements :: StateQueue a -> [a]
getElements = reverse . elements

{-# INLINE empty #-}
empty :: StateQueue a
empty = StateQueue
    { elements = []
    , ids = IntSet.empty
    }

{-# INLINE insert #-}
insertUnique
    :: Int
    -> a
    -> StateQueue a
    -> StateQueue a
insertUnique i v sq@StateQueue { ids = ids, elements = elements } =
    if i `IntSet.member` ids
        then sq
        else sq { elements = v : elements
                , ids = IntSet.insert i ids
                }

insert
    :: a
    -> StateQueue a
    -> StateQueue a
insert v sq =
    sq { elements = v : elements sq }

{-# INLINE fold #-}
fold :: (a -> x -> a) -> a -> StateQueue x -> a
fold f acc0 sq = foldl' f acc0 (reverse $ elements sq)
