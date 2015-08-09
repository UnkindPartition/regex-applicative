--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative.ListLike
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- To get started, see some examples on the wiki:
-- <https://github.com/feuerbach/regex-applicative/wiki/Examples>
--------------------------------------------------------------------
module Text.Regex.Applicative.ListLike
    ( GenRE
    , TRE
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
import Text.Regex.Applicative.Interface
import Control.Applicative
