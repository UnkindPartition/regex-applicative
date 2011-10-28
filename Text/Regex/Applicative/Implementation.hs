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


run :: StateQueue (Thread s r)
    -> [s] -> Maybe r
run queue [] = fold f Nothing queue
    where f a@Just{} _ = a
          f Nothing  x | Accept r <- x = Just r
                       | otherwise = Nothing
run queue (s:ss) =
    let accum q t =
            case t of
                Accept {} -> q
                Thread _ c ->
                    foldl (\q x -> insertThread x q) q $ c s
        newQueue = fold accum empty queue
    in run newQueue ss

match :: Regexp s a r -> [s] -> Maybe r
match r s =
    let (rr, ThreadId numStates) = renumber r
        threads = compile rr (\x -> [Accept x])
        q = foldl (\q t -> insertThread t q) empty threads
    in run q s

insertThread t q =
    case threadId t of
        ThreadId i -> insert i t q

-- This turns out to be much faster than the standard foldM,
-- because of inlining.
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a l = foldr (\x k a -> f a x >>= k) return l $ a
