{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Text.Regex.Applicative.Types

compile :: Compiled s a -> (a -> [Thread s r]) -> [Thread s r]
compile (Compiled compile2) k = compile2 k k

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

instance Regexp Compiled where
        reEps = Compiled $ \ke _kn -> ke $ error "empty"
        reSymbol i p = Compiled $ \_ke kn -> [t kn] where
          -- t :: (a -> [Thread s r]) -> Thread s r
          t k = Thread i $ \s ->
            if p s then k s else []
        reApp n1 n2 = Compiled $
            let a1 = getCompiled n1
                a2 = getCompiled n2
            in \ke kn ->
            a1
                -- empty
                (\a1_value -> a2 (ke . a1_value) (kn . a1_value))
                -- non-empty
                (\a1_value -> a2 (kn . a1_value) (kn . a1_value))
        reAlt n1 n2 = Compiled $
            let a1 = getCompiled n1
                a2 = getCompiled n2
            in \ke kn -> a1 ke kn ++ a2 ke kn
        reFmap f n = Compiled $ let a = getCompiled n in \ke kn -> a (ke . f) (kn . f)
        -- This is actually the point where we use the difference between
        -- continuations. For the inner Regexp the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        reRep g f b n = Compiled $
            let a = getCompiled n
                combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads b ke kn =
                    combine
                        (a (\_ -> []) (\v -> let b' = f b v in threads b' kn kn))
                        (ke b)
            in threads b
        reVoid n = Compiled $ let a = getCompiled n in \ke kn -> a (const $ ke ()) (const $ kn ())
        -- Void -> let a = compile2_ n in \ke kn -> a (ke ()) (kn ())

{-
compile2_ :: forall a s r . Regexp s a -> [Thread s r] -> [Thread s r] -> [Thread s r]
compile2_ e =
    case e of
        Eps -> \ke _kn -> ke
        Symbol i p -> \_ke kn -> [t kn] where
          t :: [Thread s r] -> Thread s r
          t k = Thread i $ \s ->
            if p s then k else []
        App n1 n2 ->
            let a1 = compile2_ n1
                a2 = compile2_ n2
            in \ke kn ->
            a1
                -- empty
                (a2 ke kn)
                -- non-empty
                (a2 kn kn)
        Alt n1 n2 ->
            let a1 = compile2_ n1
                a2 = compile2_ n2
            in \ke kn -> a1 ke kn ++ a2 ke kn
        Fmap f n -> compile2_ n
        -- This is actually the point where we use the difference between
        -- continuations. For the inner Regexp the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g _f b n ->
            let a = compile2_ n
                combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads ke kn = combine (a [] (threads kn kn)) ke
            in threads
        Void n -> compile2_ n
-}
