--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative.Reference
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- Reference implementation (using backtracking).
--
-- This is exposed for testing purposes only!
--------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module Text.Regex.Applicative.Reference (reference) where
import Prelude hiding (getChar, null, head, tail)
import Text.Regex.Applicative.Types
import Control.Applicative
import Control.Monad
import Data.ListLike (ListLike, null, head, tail)


-- A simple parsing monad
newtype P l s a = P { unP :: l -> [(a, l)] }

instance Monad (P l s) where
    return x = P $ \s -> [(x, s)]
    (P a) >>= k = P $ \s ->
        a s >>= \(x,s) -> unP (k x) s

instance Functor (P l s) where
    fmap = liftM

instance Applicative (P l s) where
    (<*>) = ap
    pure = return

instance Alternative (P l s) where
    empty = P $ const []
    P a1 <|> P a2 = P $ \s ->
        a1 s ++ a2 s

getChar :: ListLike l s => P l s s
getChar = P $ \s ->
  if null s
     then []
     else [(head s, tail s)]

re2monad :: ListLike l s => GenRE l s a -> P l s a
re2monad r =
    case r of
        Eps -> return $ error "eps"
        Symbol _ p -> do
            c <- getChar
            case p c of
              Just r -> return r
              Nothing -> empty
        Alt a1 a2 -> re2monad a1 <|> re2monad a2
        App a1 a2 -> re2monad a1 <*> re2monad a2
        Fmap f a -> fmap f $ re2monad a
        Rep g f b a -> rep b
            where
            am = re2monad a
            rep b = combine (do a <- am; rep $ f b a) (return b)
            combine a b = case g of Greedy -> a <|> b; NonGreedy -> b <|> a
        Void a -> re2monad a >> return ()
        Fail -> empty

runP :: ListLike l s => P l s a -> l -> Maybe a
runP m s = case filter (null . snd) $ unP m s of
    (r, _) : _ -> Just r
    _ -> Nothing

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
--
-- However, this is not very efficient implementation and is supposed to be
-- used for testing only.
reference :: ListLike l s => GenRE l s a -> l -> Maybe a
reference r s = runP (re2monad r) s
