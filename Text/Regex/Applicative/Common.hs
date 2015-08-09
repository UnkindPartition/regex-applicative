-- |
-- Collection of commonly used regular expressions.
module Text.Regex.Applicative.Common (
    -- * Digits
    digit
  , hexDigit
    -- * Numbers
  , signed
  , decimal
  , hexadecimal
  ) where

import Data.Char
import Data.List (foldl')
import Control.Applicative
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Interface

-- | Decimal digit, i.e. @\'0\'@..@\'9\'@
digit :: Num a => GenRE l Char a
digit = fromIntegral . digitToInt <$> psym isDigit

-- | Hexadecimal digit
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
hexDigit :: Num a => GenRE l Char a
hexDigit = fromIntegral . digitToInt <$> psym isHexDigit

-- | Add optional sign
signed :: Num a => GenRE l Char a -> GenRE l Char a
signed p = sign <*> p
  where
    sign =  id     <$ sym '+'
        <|> negate <$ sym '-'
        <|> pure id

-- | Parse decimal number without sign.
decimal :: Num a => GenRE l Char a
decimal = foldl' (\d i -> d*10 + i) 0 <$> some digit

-- | Parse decimal number without sign.
hexadecimal :: Num a => GenRE l Char a
hexadecimal = foldl' (\d i -> d*16 + i) 0 <$> some hexDigit
