{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Applicative.StateQueue
    ( StateQueue
    , empty
    , insert
    , member
    , fold
    , clear
    ) where

import Prelude hiding (read, lookup, replicate)
import Data.Vector.Mutable hiding (clear)
import Control.Monad
import Control.Monad.ST

data IndexedValue a = IndexedValue
    { ixKey :: !Int
    , _ixValue :: !a
    }

data StateQueue s a = StateQueue
    { dense :: !(MVector s (IndexedValue a))
    , sparseToDense :: !(MVector s Int)
    , size :: !Int
    }

{-# INLINE empty #-}
empty :: Int -> ST st (StateQueue st a)
empty maxSize = do
    d <- replicate maxSize (IndexedValue 0 $ error "SQ: Uninitialized value")
    s2d <- replicate maxSize 0
    return StateQueue
        { dense = d
        , sparseToDense = s2d
        , size = 0
        }

{-# INLINE insert #-}
insert
    :: Int -> a -> StateQueue st a
    -> ST st (StateQueue st a)
insert i v sq@StateQueue { size = size } = do
    write (sparseToDense sq) i size
    write (dense sq) size (IndexedValue i v)
    return $ sq { size = size + 1 }

{-# INLINE member #-}
member
    :: Int -> StateQueue st a -> ST st Bool
member i StateQueue {..} = {-# SCC "member" #-} do
    di <- read sparseToDense i
    if (di >= size) then return False else do
    IndexedValue { ixKey = dvKey } <- read dense di
    return $ dvKey == i

{-# INLINE fold #-}
fold :: (a -> Int -> x -> ST st a) -> a -> StateQueue st x -> ST st a
fold f acc0 sq = foldM step acc0 [0 .. size sq - 1]
  where
    step acc n = do
        IndexedValue i v <- read (dense sq) n
        f acc i v

{-# INLINE clear #-}
clear sq = sq { size = 0 }
