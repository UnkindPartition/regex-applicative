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
import Text.Regex.Applicative


-- | Decimal digit, i.e. @\'0\'..\'9\'@
digit :: Num a => RE Char a
digit = fromIntegral . digitToInt <$> psym isDigit

-- | Hexadecimal digit
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
hexDigit :: Num a => RE Char a
hexDigit = fromIntegral . digitToInt <$> psym isHexDigit

-- | Add optional sign
signed :: Num a => RE Char a -> RE Char a
signed p = sign <*> p
  where
    sign =  id     <$ sym '+'
        <|> negate <$ sym '-'
        <|> pure id

-- | Parse decimal number without sign.
decimal :: Num a => RE Char a
decimal = foldl (\d i -> d*10 + i) 0 <$> some digit

-- | Parse decimal number without sign.
hexadecimal :: Num a => RE Char a
hexadecimal = foldl (\d i -> d*16 + i) 0 <$> some hexDigit
