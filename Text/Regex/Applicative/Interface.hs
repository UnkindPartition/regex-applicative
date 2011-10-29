{-# LANGUAGE Rank2Types, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Regex.Applicative.Interface where
import Control.Applicative hiding (empty)
import qualified Control.Applicative
import Data.Traversable
import Data.String
import Data.Maybe
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Object

instance Functor (RE s) where
    fmap f x = Fmap f x

instance Applicative (RE s) where
    pure x = const x <$> Eps
    a1 <*> a2 = App a1 a2

instance Alternative (RE s) where
    a1 <|> a2 = Alt a1 a2
    empty = Eps
    many a = reverse <$> Rep (flip (:)) [] a

instance (char ~ Char, string ~ String) => IsString (RE char string) where
    fromString = string

-- | Match and return a single symbol which satisfies the predicate
psym :: (s -> Bool) -> RE s s
psym p = Symbol (error "Not numbered symbol") p

-- | Match and return the given symbol
sym :: Eq s => s -> RE s s
sym s = psym (s ==)

-- | Match and return any single symbol
anySym :: RE s s
anySym = psym (const True)

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
string :: Eq a => [a] -> RE a [a]
string = traverse sym

-- | Greedily match zero or more symbols, which are combined using the given
-- folding function
reFoldl :: (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl f b a = Rep f b a

-- | @s =~ a = match a s@
(=~) :: [s] -> RE s a -> Maybe a
s =~ a = match a s
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
match :: RE s a -> [s] -> Maybe a
match re str =
    listToMaybe $
    results $
    foldl (flip step) (compile re) str

-- | Find a string prefix which is matched by the regular expression.
--
-- Of all matching prefixes, pick one using left bias (prefer the left part of
-- '<|>' to the right part) and greediness.
--
-- This is the match which a backtracking engine (such as Perl's one) would find
-- first.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix ("a" <|> "ab") "abc"
-- >Just "a"
-- >Text.Regex.Applicative> findFirstPrefix ("ab" <|> "a") "abc"
-- >Just "ab"
-- >Text.Regex.Applicative> findFirstPrefix "bc" "abc"
-- >Nothing
findFirstPrefix :: RE s a -> [s] -> Maybe a
findFirstPrefix re str = go (compile re) str Nothing
    where
    walk obj [] = (obj, Nothing)
    walk obj (t:ts) =
        case getResult t of
            Just r -> (obj, Just r)
            Nothing -> walk (addThread t obj) ts

    go obj str resOld =
        case walk emptyObject $ threads obj of
            (obj', resThis) ->
                let res = resThis <|> resOld
                in
                    case str of
                        [] -> res
                        _ | failed obj' -> res
                        (s:ss) -> go (step s obj') ss res

-- | Find the longest string prefix which is matched by the regular expression.
--
-- Submatches are still determined using left bias and greediness, so this is
-- different from POSIX semantics.
--
-- Examples:
--
-- >Text.Regex.Applicative Data.Char> let keyword = "if"
-- >Text.Regex.Applicative Data.Char> let identifier = many $ psym isAlpha
-- >Text.Regex.Applicative Data.Char> let lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "if foo"
-- >Just (Left "if")
-- >Text.Regex.Applicative Data.Char> findLongestPrefix lexeme "iffoo"
-- >Just (Right "iffoo")
findLongestPrefix :: RE s a -> [s] -> Maybe a
findLongestPrefix re str = go (compile re) str Nothing
    where
    go obj str resOld =
        let res = (listToMaybe $ results obj) <|> resOld
        in
            case str of
                [] -> res
                _ | failed obj -> res
                (s:ss) -> go (step s obj) ss res

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: RE s a -> [s] -> Maybe a
findShortestPrefix re str = go (compile re) str
    where
    go obj str =
        case results obj of
            r : _ -> Just r
            [] ->
                case str of
                    [] -> Nothing
                    _ | failed obj -> Nothing
                    s:ss -> go (step s obj) ss
