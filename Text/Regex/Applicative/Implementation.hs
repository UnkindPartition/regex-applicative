{-# LANGUAGE GADTs, TypeFamilies, ViewPatterns, PatternGuards #-}
module Text.Regex.Applicative.Implementation (match, Regexp(..)) where
import Prelude
import Control.Applicative hiding (empty)
import Control.Monad.State hiding (foldM)
import Text.Regex.Applicative.StateQueue
import Control.Monad.ST
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Compile

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


threadId :: Thread s a -> ThreadId
threadId Accept {} = 0
threadId Thread { threadId_ = i } = i


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
