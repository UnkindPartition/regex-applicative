{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module StateQueue where

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative
import Text.Regex.Applicative.StateQueue as SQ

fromElems :: [(a, Maybe Int)] -> StateQueue a
fromElems = foldl f SQ.empty
  where
    f sq (x, mbKey) = maybe insert insertUnique mbKey x sq

size :: StateQueue a -> Int
size = length . getElements

instance (Monad m, Serial m a) => Serial m (StateQueue a) where
  series = fromElems <$> series

stateQueueTests = testGroup "StateQueue"
  [ testProperty "Insertion increments the # of elements" $
      \sq (i :: Int) -> size (insert i sq) == size sq + 1
  , testProperty "insertUnique increments the # of elements by 0 or 1" $
      \sq (i :: Int) ->
        let d = size (insertUnique i i sq) - size sq
        in d == 0 || d == 1
  , testProperty "insertUnique is idempotent" $
      \sq (i :: Int) ->
        let f = insertUnique i i
        in f sq == (f . f) sq
  , testProperty "insert doesn't affect insertUnique" $
      \(i :: Int) -> exists $ \sq ->
        let sq' = insert i sq
        in insertUnique i i sq' /= sq'
  , testProperty "insertUnique only cares about keys, not values" $
      \sq i j ->
        let sq' = insertUnique i i sq
        in insertUnique i j sq' == sq'
  , testProperty "insert puts in the back" $
      \sq (i :: Int) ->
        let sq' = insert i sq
        in last (getElements sq') == i
  , testProperty "insertUnique puts in the back" $
      \sq i ->
        let sq' = insertUnique i i sq
        in sq' /= sq ==> last (getElements sq') == i
  ]
