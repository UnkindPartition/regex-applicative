{-# LANGUAGE GADTs #-}
module Reference (reference) where
import Prelude hiding (getChar)
import RegExp hiding (empty)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe

-- Reference implementation (using backtracking)

-- A simple parsing monad
newtype P s a = P { unP :: [s] -> [(a, [s])] }

instance Monad (P s) where
    return x = P $ \s -> [(x, s)]
    (P a) >>= k = P $ \s ->
        a s >>= \(x,s) -> unP (k x) s

instance Functor (P s) where
    fmap = liftM

instance Applicative (P s) where
    (<*>) = ap
    pure = return

instance Alternative (P s) where
    empty = P $ const []
    P a1 <|> P a2 = P $ \s ->
        a1 s ++ a2 s

getChar :: P s s
getChar = P $ \s ->
    case s of
        [] -> []
        c:cs -> [(c,cs)]

re2monad :: RegexpNode s r a -> P s a
re2monad r =
    case reg r of
        Eps -> return $ error "eps"
        Symbol p -> do
            c <- getChar
            if p c then return c else empty
        Alt a1 a2 -> re2monad a1 <|> re2monad a2
        App a1 a2 -> re2monad a1 <*> re2monad a2
        Fmap f a -> fmap f $ re2monad a
        Rep f b a -> rep b
            where
            am = re2monad a
            rep b = (do a <- am; rep $ f b a) <|> return b

runP :: P s a -> [s] -> Maybe a
runP m s = case filter (null . snd) $ unP m s of
    (r, _) : _ -> Just r
    _ -> Nothing

reference :: RE s a -> [s] -> Maybe a
reference (RE r) s = runP (re2monad r) s
