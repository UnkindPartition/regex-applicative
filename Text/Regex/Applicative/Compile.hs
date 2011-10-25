{-# LANGUAGE GADTs, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile where

import Text.Regex.Applicative.Types

-- The whole point of this module is this function, compile, which needs to be
-- compiled with -fno-do-lambda-eta-expansion for efficiency.
--
-- Since this option would make other code perform worse, we place this
-- function in a separate module and make sure it's not inlined.
--
-- The point of "-fno-do-lambda-eta-expansion" is to make sure the tree is
-- "compiled" only once.
compile :: forall a s r . Regexp s ThreadId a -> (a -> [Thread s r]) -> [Thread s r]
compile e =
    case e of
        Eps -> \k -> k $ error "empty"
        Symbol i p -> \k -> [t k] where
          t :: (a -> [Thread s r]) -> Thread s r
          t k = Thread i $ \s ->
            if p s then k s else []
        App (compile -> a1) (compile -> a2) -> \k ->
            a1 (\a1_value -> a2 (k . a1_value))
        Alt (compile -> a1) (compile -> a2) ->
            \k -> a1 k ++ a2 k
        Fmap f (compile -> a) -> \k -> a (k . f)
        Rep f b (compile -> a) -> \k ->
            let threads b =
                    a (\v -> let b' = f b v in threads b') ++ k b
            in threads b
{-# NOINLINE compile #-}
