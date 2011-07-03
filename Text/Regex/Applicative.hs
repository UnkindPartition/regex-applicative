--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
--------------------------------------------------------------------

module Text.Regex.Applicative
    ( RE
    , sym
    , psym
    , anySym
    , reFoldl
    , (=~)
    , module Control.Applicative
    )
    where
import Text.Regex.Applicative.Interface
import Control.Applicative
