{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Applicative.StateQueue
    ( StateQueue
    , empty
    , insert
    , fold
    ) where

import Prelude hiding (read, lookup, replicate)
import qualified Data.IntSet as IntSet

data StateQueue a = StateQueue
    { elements :: [a]
    , ids :: IntSet.IntSet
    }

{-# INLINE empty #-}
empty :: StateQueue a
empty = StateQueue
    { elements = []
    , ids = IntSet.empty
    }

{-# INLINE insert #-}
insert
    :: Int
    -> a
    -> StateQueue a
    -> StateQueue a
insert i v sq@StateQueue {..} =
    if i `IntSet.member` ids
        then sq
        else sq { elements = v : elements
                , ids = IntSet.insert i ids
                }

{-# INLINE fold #-}
fold :: (a -> x -> a) -> a -> StateQueue x -> a
fold f acc0 sq = foldl f acc0 (reverse $ elements sq)
