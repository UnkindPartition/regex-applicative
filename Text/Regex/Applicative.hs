{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- To get started, see some examples on the wiki:
-- <https://github.com/feuerbach/regex-applicative/wiki/Examples>
--------------------------------------------------------------------

module Text.Regex.Applicative
    ( RE
    , sym
    , psym
    , msym
    , anySym
    , string
    , reFoldl
    , Greediness(..)
    , few
    , comap
    , withMatched
    , match
    , (=~)
    , findFirstPrefix
    , findLongestPrefix
    , findShortestPrefix
    , findFirstInfix
    , findLongestInfix
    , findShortestInfix
    , module Control.Applicative
    )
    where
import Text.Regex.Applicative.Types
import qualified Text.Regex.Applicative.Interface as I
import Control.Applicative


-- | 'RE' is a kind-of profunctor. This is its contravariant map.
--
-- (A dependency on the @profunctors@ package doesn't seem justified.)
comap :: (s2 -> s1) -> RE [s1] s1 a -> RE [s2] s2 a
comap f re =
  case re of
    Eps -> Eps
    Symbol t p    -> Symbol t (p . f)
    Alt r1 r2     -> Alt (comap f r1) (comap f r2)
    App r1 r2     -> App (comap f r1) (comap f r2)
    Fmap g r      -> Fmap g (comap f r)
    Fail          -> Fail
    Rep gr fn a r -> Rep gr fn a (comap f r)
    Void r        -> Void (comap f r)

-- | Match and return a single symbol which satisfies the predicate
psym :: (s -> Bool) -> RE [s] s s
psym = I.psym

-- | Like 'psym', but allows to return a computed value instead of the
-- original symbol
msym :: (s -> Maybe a) -> RE l s a
msym = I.msym

-- | Match and return the given symbol
sym :: Eq s => s -> RE [s] s s
sym = I.sym

-- | Match and return any single symbol
anySym :: RE [s] s s
anySym = I.anySym

-- | Match and return the given sequence of symbols.
--
-- Note that there is an 'IsString' instance for regular expression, so
-- if you enable the @OverloadedStrings@ language extension, you can write
-- @string \"foo\"@ simply as @\"foo\"@.
--
-- Example:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Text.Regex.Applicative
-- >
-- >number = "one" *> pure 1  <|>  "two" *> pure 2
-- >
-- >main = print $ "two" =~ number
string :: Eq a => [a] -> RE [s] a [a]
string = I.string

-- | Match zero or more instances of the given expression, which are combined using
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
reFoldl :: Greediness -> (b -> a -> b) -> b -> RE [s] s a -> RE [s] s b
reFoldl = I.reFoldl

-- | Match zero or more instances of the given expression, but as
-- few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- is 'many'.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix (few anySym  <* "b") "ababab"
-- >Just ("a","abab")
-- >Text.Regex.Applicative> findFirstPrefix (many anySym  <* "b") "ababab"
-- >Just ("ababa","")
few :: RE l s a -> RE l s [a]
few = I.few


-- | Return matched symbols as part of the return value
withMatched :: RE [s] s a -> RE [s] s (a, [s])
withMatched = I.withMatched

-- | @s =~ a = match a s@
(=~) :: [s] -> RE [s] s a -> Maybe a
(=~) = (I.=~)
infix 2 =~

-- | Attempt to match a string of symbols against the regular expression.
-- Note that the whole string (not just some part of it) should be matched.
--
-- Examples:
--
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "a"
-- >Just 'a'
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "ab"
-- >Nothing
--
match :: RE [s] s a -> [s] -> Maybe a
match = I.match

-- | Find a string prefix which is matched by the regular expression.
--
-- Of all matching prefixes, pick one using left bias (prefer the left part of
-- '<|>' to the right part) and greediness.
--
-- This is the match which a backtracking engine (such as Perl's one) would find
-- first.
--
-- If match is found, the rest of the input is also returned.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix ("a" <|> "ab") "abc"
-- >Just ("a","bc")
-- >Text.Regex.Applicative> findFirstPrefix ("ab" <|> "a") "abc"
-- >Just ("ab","c")
-- >Text.Regex.Applicative> findFirstPrefix "bc" "abc"
-- >Nothing
findFirstPrefix :: RE [s] s a -> [s] -> Maybe (a, [s])
findFirstPrefix = I.findFirstPrefix

-- | Find the longest string prefix which is matched by the regular expression.
--
-- Submatches are still determined using left bias and greediness, so this is
-- different from POSIX semantics.
--
-- If match is found, the rest of the input is also returned.
--
-- Examples:
--
-- >Text.Regex.Applicative Data.Char> let keyword = "if"
-- >Text.Regex.Applicative Data.Char> let identifier = many $ psym isAlpha
-- >Text.Regex.Applicative Data.Char> let lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "if foo"
-- >Just (Left "if"," foo")
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "iffoo"
-- >Just (Right "iffoo","")
findLongestPrefix :: RE [s] s a -> [s] -> Maybe (a, [s])
findLongestPrefix = I.findLongestPrefix

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: RE [s] s a -> [s] -> Maybe (a, [s])
findShortestPrefix = I.findShortestPrefix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findFirstInfix :: RE [s] s a -> [s] -> Maybe ([s], a, [s])
findFirstInfix = I.findFirstInfix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findLongestInfix :: RE [s] s a -> [s] -> Maybe ([s], a, [s])
findLongestInfix = I.findLongestInfix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findShortestInfix :: RE [s] s a -> [s] -> Maybe ([s], a, [s])
findShortestInfix = I.findShortestInfix
