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
-- <https://github.com/UnkindPartition/regex-applicative/wiki/Examples>
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
    , replace
    , findFirstPrefix
    , findLongestPrefix
    , findShortestPrefix
    , findFirstInfix
    , findLongestInfix
    , findShortestInfix
    -- * Custom uncons function
    -- $uncons
    , findFirstPrefixWithUncons
    , findLongestPrefixWithUncons
    , findShortestPrefixWithUncons
    , module Control.Applicative
    )
    where
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Interface
import Control.Applicative

{- $uncons
The following functions take an argument that splits the input into the first symbol and
the remaining input (if the input is non-empty).

It is useful, for example, for feeding a @Text@ to a regex matcher:

>>> findFirstPrefixWithUncons Text.uncons (many (sym 'a')) "aaa"
Just ("aaa", "")

For another example, feeding input symbols annotated with source positions into a matcher,
preserving the positions in the remaining input so the location of a lexical error can be
recovered:

@
data AList a b = AList { annotation :: a, stripAnnotation :: Maybe (b, AList a b) }

findLongestPrefixAnnotated :: RE s a -> AList b s -> Maybe (a, AList b s)
fondLongestPrefixAnnotated = findLongestPrefixWithUncons stripAnnotation
@

The use of the other functions taking an @uncons@ argument is exactly analogous.
-}
