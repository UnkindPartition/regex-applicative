{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Prelude hiding ((.))
import Text.Regex.Applicative.Types

compile :: RE s a -> (a -> [Thread s r]) -> [Thread s r]
compile e k = compile2 e (SingleCont k)

infixr 9 .
(f . g) x = f $! g x

data Cont a = SingleCont !a | EmptyNonEmpty !a !a

instance Functor Cont where
    fmap f k =
        case k of
            SingleCont a -> SingleCont (f a)
            EmptyNonEmpty a b -> EmptyNonEmpty (f a) (f b)

emptyCont :: Cont a -> a
emptyCont k =
    case k of
        SingleCont a -> a
        EmptyNonEmpty a _ -> a
nonEmptyCont :: Cont a -> a
nonEmptyCont k =
    case k of
        SingleCont a -> a
        EmptyNonEmpty _ a -> a

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
compile2 :: RE s a -> Cont (a -> [Thread s r]) -> [Thread s r]
compile2 e =
    case e of
        Eps -> \k -> emptyCont k $ error "empty"
        Symbol i p -> \k -> [t $ nonEmptyCont k] where
          -- t :: (a -> [Thread s r]) -> Thread s r
          t k = Thread i $ \s ->
            if p s then k s else []
        App n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \k -> case k of
                SingleCont k -> a1 $ SingleCont $ \a1_value -> a2 $ SingleCont $ k . a1_value
                EmptyNonEmpty ke kn ->
                    a1 $ EmptyNonEmpty
                        -- empty
                        (\a1_value -> a2 $ EmptyNonEmpty (ke . a1_value) (kn . a1_value))
                        -- non-empty
                        (\a1_value -> a2 $ EmptyNonEmpty (kn . a1_value) (kn . a1_value))
        Alt n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \k -> a1 k ++ a2 k
        Fmap f n -> let a = compile2 n in \k -> a $ fmap (. f) k
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b n ->
            let a = compile2 n
                combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads b k =
                    combine
                        (a $ EmptyNonEmpty (\_ -> []) (\v -> let b' = f b v in threads b' (SingleCont $ nonEmptyCont k)))
                        (emptyCont k b)
            in threads b
        Void n -> let a = compile2_ n in \k -> a $ fmap ($ ()) k

compile2_ :: RE s a -> Cont [Thread s r] -> [Thread s r]
compile2_ e =
    case e of
        Eps -> \k -> emptyCont k
        Symbol i p -> \k -> [t $ nonEmptyCont k] where
          -- t :: [Thread s r] -> Thread s r
          t k = Thread i $ \s ->
            if p s then k else []
        App n1 n2 ->
            let a1 = compile2_ n1
                a2 = compile2_ n2
            in \k ->
                case k of
                    SingleCont {} -> a1 $ SingleCont $ a2 k
                    EmptyNonEmpty ke kn ->
                        a1 $ EmptyNonEmpty
                            -- empty
                            (a2 $ EmptyNonEmpty ke kn)
                            -- non-empty
                            (a2 $ EmptyNonEmpty kn kn)
        Alt n1 n2 ->
            let a1 = compile2_ n1
                a2 = compile2_ n2
            in \k -> a1 k ++ a2 k
        Fmap _ n -> compile2_ n
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g _ _ n ->
            let a = compile2_ n
                combine continue stop =
                    case g of
                        Greedy -> continue ++ stop
                        NonGreedy -> stop ++ continue
                threads k = combine (a $ EmptyNonEmpty [] (threads $ SingleCont $ nonEmptyCont k)) (emptyCont k)
            in threads
        Void n -> compile2_ n
