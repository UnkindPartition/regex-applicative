{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Control.Monad.Trans.State
import Text.Regex.Applicative.Types
import Control.Applicative
import Data.Maybe
import qualified Data.IntMap as IntMap

compile :: RE s a -> (a -> [Thread s r]) -> [Thread s r]
compile e k = compile2 e (SingleCont k)

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
        Eps -> \k -> emptyCont k ()
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
        Fail -> const []
        Fmap f n -> let a = compile2 n in \k -> a $ fmap (. f) k
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b n ->
            let a = compile2 n
                threads b k =
                    combine g
                        (a $ EmptyNonEmpty (\_ -> []) (\v -> let b' = f b v in threads b' (SingleCont $ nonEmptyCont k)))
                        (emptyCont k b)
            in threads b
        Void n -> let a = compile2_ n in \k -> a $ fmap ($ ()) k

data FSMState
    = SAccept
    | STransition ThreadId

type FSMMap s = IntMap.IntMap (s -> Bool, [FSMState])

mkNFA :: RE s a -> ([FSMState], (FSMMap s))
mkNFA e =
    flip runState IntMap.empty $
        go e [SAccept]
  where
  go :: RE s a -> [FSMState] -> State (FSMMap s) [FSMState]
  go e k =
    case e of
        Eps -> return k
        Symbol i@(ThreadId n) p -> do
            modify $ IntMap.insert n $
                (p, k)
            return [STransition i]
        App n1 n2 -> go n1 =<< go n2 k
        Alt n1 n2 -> (++) <$> go n1 k <*> go n2 k
        Fail -> return []
        Fmap _ n -> go n k
        Rep g _ _ n ->
            let entries = findEntries n
                cont = combine g entries k
            in
            -- return value of 'go' is ignored -- it should be a subset of
            -- 'cont'
            go n cont >> return cont
        Void n -> go n k

  findEntries :: RE s a -> [FSMState]
  findEntries e =
    -- A simple (although a bit inefficient) way to find all entry points is
    -- just to use 'go'
    evalState (go e []) IntMap.empty

compile2_ :: RE s a -> Cont [Thread s r] -> [Thread s r]
compile2_ e =
    let (entries, fsmap) = mkNFA e
        mkThread _ k1 (STransition i@(ThreadId n)) =
            let (p, cont) = fromMaybe (error "Unknown id") $ IntMap.lookup n fsmap
            in [Thread i $ \s ->
                if p s
                    then concatMap (mkThread k1 k1) cont
                    else []]
        mkThread k0 _ SAccept = k0

    in \k -> concatMap (mkThread (emptyCont k) (nonEmptyCont k)) entries

combine g continue stop =
    case g of
        Greedy -> continue ++ stop
        NonGreedy -> stop ++ continue
