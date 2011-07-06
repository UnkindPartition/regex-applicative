{-# LANGUAGE Rank2Types #-}
module Text.Regex.Applicative.Interface where
import Control.Applicative hiding (empty)
import qualified Control.Applicative
import Data.Traversable
import Text.Regex.Applicative.Implementation
import Text.Regex.Applicative.Priorities

-- | Type of regular expressions that recognize symbols of type @s@ and
-- produce a result of type @a@.
--
-- Regular expressions can be built using 'Functor', 'Applicative' and
-- 'Alternative' instances in the following natural way:
--
-- * @f@ '<$>' @ra@ matches iff @ra@ matches, and its return value is the result
-- of applying @f@ to the return value of @ra@.
--
-- * 'pure' @x@ matches the empty string (i.e. it does not consume any symbols),
-- and its return value is @x@
--
-- * @rf@ '<*>' @ra@ matches a string iff it is a concatenation of two
-- strings: one matched by @rf@ and the other matched by @ra@. The return value
-- is @f a@, where @f@ and @a@ are the return values of @rf@ and @ra@
-- respectively.
--
-- * @ra@ '<|>' @rb@ matches a string which is accepted by either @ra@ or @rb@.
-- It is left-biased, so if both can match, the result of @ra@ is used.
--
-- * 'Control.Applicative.empty' is a regular expression which does not match any string.
--
-- * 'many' @ra@ matches concatenation of zero or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
newtype RE s a = RE { unRE :: forall r . RegexpNode s r a }

instance Functor (RE s) where
    fmap f (RE a) = RE $ fmapNode f a

instance Applicative (RE s) where
    pure x = const x <$> RE epsNode
    (RE a1) <*> (RE a2) = RE $ appNode a1 a2

instance Alternative (RE s) where
    (RE a1) <|> (RE a2) = RE $ altNode a1 a2
    empty = error "noMatch" <$> psym (const False)
    many a = reverse <$> reFoldl (flip (:)) [] a

-- | Matches and returns a single symbol which satisfies the predicate
psym :: (s -> Bool) -> RE s s
psym p = RE $ symbolNode p

-- | Matches and returns the given symbol
sym :: Eq s => s -> RE s s
sym s = psym (s ==)

-- | Matches and returns any single symbol
anySym :: RE s s
anySym = psym (const True)

-- | Matches and returns the given sequence of symbols
string :: Eq a => [a] -> RE a [a]
string = sequenceA . map sym

-- | Greedily matches zero or more symbols, which are combined using the given
-- folding function
reFoldl :: (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl f b (RE a) = RE $ repNode f b a

-- | Attempts to match a string of symbols against the regular expression
(=~) :: [s] -> RE s a -> Maybe a
s =~ (RE r) = priorityToMaybe $ match r s
