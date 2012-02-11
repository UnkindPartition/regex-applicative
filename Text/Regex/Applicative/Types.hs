{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion -fno-warn-unused-imports #-}
module Text.Regex.Applicative.Types where

import Control.Applicative
-- The above import is needed for haddock to properly generate links to
-- Applicative methods. But it's not actually used in the code, hence
-- -fno-warn-unused-imports.


newtype ThreadId = ThreadId Int

-- | A thread either is a result or corresponds to a symbol in the regular
-- expression, which is expected by that thread.
data Thread s r
    = Thread
        { threadId_ :: ThreadId
        , _threadCont :: s -> [Thread s r]
        }
    | Accept r

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- 'Nothing' for results.
threadId :: Thread s r -> Maybe ThreadId
threadId Thread { threadId_ = i } = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy
    deriving (Show, Read, Eq, Ord, Enum)

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
-- * 'empty' is a regular expression which does not match any string.
--
-- * 'many' @ra@ matches concatenation of zero or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
--
-- * 'some' @ra@ matches concatenation of one or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
data RE s a where
    Eps :: RE s a
    Symbol :: ThreadId -> (s -> Bool) -> RE s s
    Alt :: RE s a -> RE s a -> RE s a
    App :: RE s (a -> b) -> RE s a -> RE s b
    Fmap :: (a -> b) -> RE s a -> RE s b
    Rep :: Greediness    -- repetition may be greedy or not
        -> (b -> a -> b) -- folding function (like in foldl)
        -> b             -- the value for zero matches, and also the initial value
                         -- for the folding function
        -> RE s a
        -> RE s b
    Void :: RE s a -> RE s ()
