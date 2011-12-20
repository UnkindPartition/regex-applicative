{-# LANGUAGE GADTs, ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Text.Regex.Applicative.Types

compile :: forall a s r . RE s a -> (a -> [Thread s r]) -> [Thread s r]
compile e k = compile2 e k k

-- The whole point of this module is this function, compile2, which needs to be
-- compiled with -fno-do-lambda-eta-expansion for efficiency.
--
-- Since this option would make other code perform worse, we place this
-- function in a separate module and make sure it's not inlined.
--
-- The point of "-fno-do-lambda-eta-expansion" is to make sure the tree is
-- "compiled" only once.
--
-- compile2 function takes two continuations: one when the match is empty and
-- one when the match is non-empty. See the "Rep" case for the reason.
compile2 :: forall a s r . RE s a -> (a -> [Thread s r]) -> (a -> [Thread s r]) -> [Thread s r]
compile2 e =
    case e of
        Eps -> \ke _kn -> ke $ error "empty"
        Symbol i p -> \_ke kn -> [t kn] where
          t :: (a -> [Thread s r]) -> Thread s r
          t k = Thread i $ \s ->
            if p s then k s else []
        App (compile2 -> a1) (compile2 -> a2) -> \ke kn ->
            a1
                -- empty
                (\a1_value -> a2 (ke . a1_value) (kn . a1_value))
                -- non-empty
                (\a1_value -> a2 (kn . a1_value) (kn . a1_value))
        Alt (compile2 -> a1) (compile2 -> a2) ->
            \ke kn -> a1 ke kn ++ a2 ke kn
        Fmap f (compile2 -> a) -> \ke kn -> a (ke . f) (kn . f)
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b (compile2 -> a) ->
            let combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads b ke kn =
                    combine
                        (a (\_ -> []) (\v -> let b' = f b v in threads b' kn kn))
                        (ke b)
            in threads b
        Void (compile2_ -> a) -> \ke kn -> a (ke ()) (kn ())

compile2_ :: forall a s r . RE s a -> [Thread s r] -> [Thread s r] -> [Thread s r]
compile2_ e =
    case e of
        Eps -> \ke _kn -> ke
        Symbol i p -> \_ke kn -> [t kn] where
          t :: [Thread s r] -> Thread s r
          t k = Thread i $ \s ->
            if p s then k else []
        App (compile2_ -> a1) (compile2_ -> a2) -> \ke kn ->
            a1
                -- empty
                (a2 ke kn)
                -- non-empty
                (a2 kn kn)
        Alt (compile2_ -> a1) (compile2_ -> a2) ->
            \ke kn -> a1 ke kn ++ a2 ke kn
        Fmap f (compile2_ -> a) -> a
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g _f b (compile2_ -> a) ->
            let combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads ke kn =
                    combine
                        (a [] (threads kn kn))
                        ke
            in threads
        Void (compile2_ -> a) -> \ke kn -> a ke kn
