{-# LANGUAGE GADTs, TypeFamilies, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, ViewPatterns, PatternGuards #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Implementation (match, Regexp(..)) where
import Prelude
import Control.Applicative hiding (empty)
import Control.Monad.State hiding (foldM)
import Text.Regex.Applicative.StateQueue
import Control.Monad.ST

newtype ThreadId = ThreadId Int
    deriving (Show, Eq, Ord, Num)

data Regexp s i a where
    Eps :: Regexp s i a
    Symbol :: i -> (s -> Bool) -> Regexp s i s
    Alt :: Regexp s i a -> Regexp s i a -> Regexp s i a
    App :: Regexp s i (a -> b) -> Regexp s i a -> Regexp s i b
    Fmap :: (a -> b) -> Regexp s i a -> Regexp s i b
    Rep :: (b -> a -> b) -- folding function (like in foldl)
        -> b             -- the value for zero matches, and also the initial value
                         -- for the folding function
        -> Regexp s i a
        -> Regexp s i b

fresh :: (MonadState m, StateType m ~ ThreadId) => m ThreadId
fresh = do
    i <- get
    put $! i+1
    return i

renumber :: Regexp s i a -> (Regexp s ThreadId a, ThreadId)
renumber e = flip runState 1 $ compile e
  where
    compile :: Regexp s i a -> State ThreadId (Regexp s ThreadId a)
    compile e =
        case e of
            Eps -> return Eps
            Symbol _ p -> Symbol <$> fresh <*> pure p
            Alt a1 a2 -> Alt <$> compile a1 <*> compile a2
            App a1 a2 -> App <$> compile a1 <*> compile a2
            Fmap f a -> Fmap f <$> compile a
            Rep f b a -> Rep f b <$> compile a

data Thread s a
    = Thread
        { threadId_ :: ThreadId
        , _threadCont :: s -> [Thread s a]
        }
    | Accept a

threadId :: Thread s a -> ThreadId
threadId Accept {} = 0
threadId Thread { threadId_ = i } = i

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
                    a (\v -> let b' = f b v in threads b' ++ k b') ++ k b
            in threads b

run :: StateQueue st (Thread s r)
    -> StateQueue st (Thread s r)
    -> [s] -> ST st (Maybe r)
run queue _ [] = fold f Nothing queue
    where f a@Just{} _ _ = return a
          f Nothing  _ x | Accept r <- x = return $ Just r
                         | otherwise = return Nothing
run queue newQueue (s:ss) = do
    let accum q _ t =
            case t of
                Accept {} -> return q
                Thread _ c ->
                    foldM (\q x -> tryInsert x q) q $ c s
    newQueue <- fold accum newQueue queue
    let veryNewQueue = clear queue
    run newQueue veryNewQueue ss

tryInsert :: Thread s r -> StateQueue st (Thread s r) -> ST st (StateQueue st (Thread s r))
tryInsert t@(threadId -> ThreadId i) queue = do
    alreadyPresent <- member i queue
    if alreadyPresent
        then return queue
        else insert i t queue

match :: Regexp s a r -> [s] -> Maybe r
match r s = runST $ do
    let (rr, ThreadId numStates) = renumber r
    q1 <- empty numStates
    q2 <- empty numStates
    let threads = compile rr (\x -> [Accept x])
    q1 <- foldM (\q t -> tryInsert t q) q1 threads
    run q1 q2 s

-- This turns out to be much faster than the standard foldM,
-- because of inlining.
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a l = foldr (\x k a -> f a x >>= k) return l $ a
