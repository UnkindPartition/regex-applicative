{-# LANGUAGE TypeFamilies, GADTs, TupleSections #-}
module Text.Regex.Applicative.Interface where
import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad (guard)
import qualified Data.List as List
import Data.Maybe
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Object

-- | 'RE' is a profunctor. This is its contravariant map.
--
-- (A dependency on the @profunctors@ package doesn't seem justified.)
comap :: (s2 -> s1) -> RE s1 a -> RE s2 a
comap f re =
  case re of
    Eps -> Eps
    Symbol t p    -> Symbol t (p . f)
    Alt r1 r2     -> Alt (comap f r1) (comap f r2)
    App r1 r2     -> App (comap f r1) (comap f r2)
    Fmap g r      -> Fmap g (comap f r)
    CatMaybes r   -> CatMaybes (comap f r)
    Fail          -> Fail
    Rep gr fn a r -> Rep gr fn a (comap f r)
    Void r        -> Void (comap f r)

-- | Match and return any single symbol
anySym :: RE s s
anySym = msym Just

-- | Match zero or more instances of the given expression, which are combined using
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
reFoldl :: Greediness -> (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl g f b a = Rep g f b a

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
few :: RE s a -> RE s [a]
few a = reverse <$> Rep NonGreedy (flip (:)) [] a

-- | Return matched symbols as part of the return value
withMatched :: RE s a -> RE s (a, [s])
withMatched Eps = flip (,) [] <$> Eps
withMatched (Symbol t p) = Symbol t (\s -> (,[s]) <$> p s)
withMatched (Alt a b) = withMatched a <|> withMatched b
withMatched (App a b) =
    (\(f, s) (x, t) -> (f x, s ++ t)) <$>
        withMatched a <*>
        withMatched b
withMatched Fail = Fail
withMatched (Fmap f x) = (f *** id) <$> withMatched x
withMatched (CatMaybes x) = CatMaybes $
    (\ (as, s) -> flip (,) s <$> as) <$> withMatched x
withMatched (Rep gr f a0 x) =
    Rep gr (\(a, s) (x, t) -> (f a x, s ++ t)) (a0, []) (withMatched x)
-- N.B.: this ruins the Void optimization
withMatched (Void x) = (const () *** id) <$> withMatched x

-- | @s =~ a = match a s@
(=~) :: [s] -> RE s a -> Maybe a
(=~) = flip match
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
match re = let obj = compile re in \str ->
    listToMaybe $
    results $
    foldl (flip step) obj str

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
-- See also 'findFirstPrefixWithUncons', of which this is a special case.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix ("a" <|> "ab") "abc"
-- >Just ("a","bc")
-- >Text.Regex.Applicative> findFirstPrefix ("ab" <|> "a") "abc"
-- >Just ("ab","c")
-- >Text.Regex.Applicative> findFirstPrefix "bc" "abc"
-- >Nothing
findFirstPrefix :: RE s a -> [s] -> Maybe (a, [s])
findFirstPrefix = findFirstPrefixWithUncons List.uncons

-- | Find the first prefix, with the given @uncons@ function.
--
-- @since 0.3.4
findFirstPrefixWithUncons :: (ss -> Maybe (s, ss)) -> RE s a -> ss -> Maybe (a, ss)
findFirstPrefixWithUncons = findPrefixWith' (walk emptyObject . threads)
  where
    walk obj [] = (obj, Nothing)
    walk obj (t:ts) =
        case getResult t of
            Just r -> (obj, Just r)
            Nothing -> walk (addThread t obj) ts

-- | Find the longest string prefix which is matched by the regular expression.
--
-- Submatches are still determined using left bias and greediness, so this is
-- different from POSIX semantics.
--
-- If match is found, the rest of the input is also returned.
--
-- See also 'findLongestPrefixWithUncons', of which this is a special case.
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
findLongestPrefix :: RE s a -> [s] -> Maybe (a, [s])
findLongestPrefix = findLongestPrefixWithUncons List.uncons

-- | Find the longest prefix, with the given @uncons@ function.
--
-- @since 0.3.4
findLongestPrefixWithUncons :: (ss -> Maybe (s, ss)) -> RE s a -> ss -> Maybe (a, ss)
findLongestPrefixWithUncons = findPrefixWith' ((,) <*> listToMaybe . results)

findPrefixWith'
 :: (ReObject s a -> (ReObject s a, Maybe a))
 -- ^ Given the regex object, compute the regex object to feed the next input value into, and
 -- the result, if any.
 -> (ss -> Maybe (s, ss)) -- ^ @uncons@
 -> RE s a -> ss -> Maybe (a, ss)
findPrefixWith' walk uncons = \ re -> go (compile re) Nothing
  where
    go obj resOld ss = case walk obj of
        (obj', resThis) ->
            let res = flip (,) ss <$> resThis <|> resOld
            in
                case uncons ss of
                    _ | failed obj' -> res
                    Nothing -> res
                    Just (s, ss) -> go (step s obj') res ss

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
--
-- See also 'findShortestPrefixWithUncons', of which this is a special case.
findShortestPrefix :: RE s a -> [s] -> Maybe (a, [s])
findShortestPrefix = findShortestPrefixWithUncons List.uncons

-- | Find the shortest prefix (analogous to 'findLongestPrefix'), with the given @uncons@ function.
--
-- @since 0.3.4
findShortestPrefixWithUncons :: (ss -> Maybe (s, ss)) -> RE s a -> ss -> Maybe (a, ss)
findShortestPrefixWithUncons uncons = go . compile
  where
    go obj ss = case results obj of
        r:_ -> Just (r, ss)
        _ -> do
            guard (not (failed obj))
            (s, ss) <- uncons ss
            go (step s obj) ss

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findFirstInfix :: RE s a -> [s] -> Maybe ([s], a, [s])
findFirstInfix re str =
    fmap (\((first, res), last) -> (first, res, last)) $
    findFirstPrefix ((,) <$> few anySym <*> re) str

-- Auxiliary function for findExtremeInfix
prefixCounter :: RE s (Int, [s])
prefixCounter = second reverse <$> reFoldl NonGreedy f (0, []) anySym
    where
    f (i, prefix) s = ((,) $! (i+1)) $ s:prefix

data InfixMatchingState s a = GotResult
    { prefixLen  :: !Int
    , prefixStr  :: [s]
    , result     :: a
    , postfixStr :: [s]
    }
    | NoResult

-- a `preferOver` b chooses one of a and b, giving preference to a
preferOver
    :: InfixMatchingState s a
    -> InfixMatchingState s a
    -> InfixMatchingState s a
preferOver NoResult b = b
preferOver b NoResult = b
preferOver a b =
    case prefixLen a `compare` prefixLen b of
        GT -> b -- prefer b when it has smaller prefix
        _  -> a -- otherwise, prefer a

mkInfixMatchingState
    :: [s] -- rest of input
    -> Thread s ((Int, [s]), a)
    -> InfixMatchingState s a
mkInfixMatchingState rest thread =
    case getResult thread of
        Just ((pLen, pStr), res) ->
            GotResult
                { prefixLen = pLen
                , prefixStr = pStr
                , result    = res
                , postfixStr = rest
                }
        Nothing -> NoResult

gotResult :: InfixMatchingState s a -> Bool
gotResult GotResult {} = True
gotResult _ = False

-- Algorithm for finding leftmost longest infix match:
--
-- 1. Add a thread /.*?/ to the begginning of the regexp
-- 2. As soon as we get first accept, we delete that thread
-- 3. When we get more than one accept, we choose one by the following criteria:
-- 3.1. Compare by the length of prefix (since we are looking for the leftmost
-- match)
-- 3.2. If they are produced on the same step, choose the first one (left-biased
-- choice)
-- 3.3. If they are produced on the different steps, choose the later one (since
-- they have the same prefixes, later means longer)
findExtremalInfix
    :: -- function to combine a later result (first arg) to an earlier one (second
       -- arg)
       (InfixMatchingState s a -> InfixMatchingState s a -> InfixMatchingState s a)
    -> RE s a
    -> [s]
    -> Maybe ([s], a, [s])
findExtremalInfix newOrOld re str =
    case go (compile $ (,) <$> prefixCounter <*> re) str NoResult of
        NoResult -> Nothing
        r@GotResult{} ->
            Just (prefixStr r, result r, postfixStr r)
    where
    {-
    go :: ReObject s ((Int, [s]), a)
       -> [s]
       -> InfixMatchingState s a
       -> InfixMatchingState s a
    -}
    go obj str resOld =
        let resThis =
                foldl
                    (\acc t -> acc `preferOver` mkInfixMatchingState str t)
                    NoResult $
                    threads obj
            res = resThis `newOrOld` resOld
            obj' =
                -- If we just found the first result, kill the "prefixCounter" thread.
                -- We rely on the fact that it is the last thread of the object.
                if gotResult resThis && not (gotResult resOld)
                    then fromThreads $ init $ threads obj
                    else obj
        in
            case str of
                [] -> res
                _ | failed obj -> res
                (s:ss) -> go (step s obj') ss res


-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findLongestInfix :: RE s a -> [s] -> Maybe ([s], a, [s])
findLongestInfix = findExtremalInfix preferOver

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findShortestInfix :: RE s a -> [s] -> Maybe ([s], a, [s])
findShortestInfix = findExtremalInfix $ flip preferOver

-- | Replace matches of the regular expression with its value.
--
-- >Text.Regex.Applicative > replace ("!" <$ sym 'f' <* some (sym 'o')) "quuxfoofooooofoobarfobar"
-- >"quux!!!bar!bar"
replace :: RE s [s] -> [s] -> [s]
replace r = ($ []) . go
  where go ys = case findLongestInfix r ys of
                    Nothing                -> (ys ++)
                    Just (before, m, rest) -> (before ++) . (m ++) . go rest
