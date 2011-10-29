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

-- | Matches and returns a single symbol which satisfies the predicate
psym :: (s -> Bool) -> RE s s
psym p = Symbol (error "Not numbered symbol") p

-- | Matches and returns the given symbol
sym :: Eq s => s -> RE s s
sym s = psym (s ==)

-- | Matches and returns any single symbol
anySym :: RE s s
anySym = psym (const True)

-- | Matches and returns the given sequence of symbols.
--
-- Note that you there is an 'IsString' instance for regular expression, so
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

-- | Greedily matches zero or more symbols, which are combined using the given
-- folding function
reFoldl :: (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl f b a = Rep f b a

-- | Attempts to match a string of symbols against the regular expression
(=~) :: [s] -> RE s a -> Maybe a
s =~ a = match a s
infix 2 =~

match :: RE s a -> [s] -> Maybe a
match re str =
    listToMaybe $
    results $
    foldl (flip step) (compile re) str
