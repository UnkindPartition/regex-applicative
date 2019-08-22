regex-applicative
=================

*regex-applicative* is a parsing combinator library for Haskell based on regular
expressions.

Example
-------

``` haskell
import Text.Regex.Applicative

data Protocol = HTTP | FTP deriving Show

protocol :: RE Char Protocol
protocol = HTTP <$ string "http" <|> FTP <$ string "ftp"

type Host = String
type Location = String
data URL = URL Protocol Host Location deriving Show

host :: RE Char Host
host = many $ psym $ (/= '/')

url :: RE Char URL
url = URL <$> protocol <* string "://" <*> host <* sym '/' <*> many anySym

main = print $ "http://stackoverflow.com/questions" =~ url
```

Documentation
-------------

See the [API reference][haddock].

Performance
-----------

For common tasks, this package is several times slower than monadic
parser combinator libraries like parsec. However, this library has a roughly
linear complexity, whereas monadic parser combinators have exponential
worst-time complexity (see [here](https://swtch.com/~rsc/regexp/regexp1.html)).

Some tips to make your regex run faster:

1. If you don't care about the result of the whole regex or its part, only
   whether it matches or not, mark it with `void` or `<$`. Recognition is faster
   than parsing.
1. If you apply the same regex to multiple strings, partially apply it like so:

   ```
   let matcher = match my_regex
   in  map matcher my_strings
   ```

   This way the compiled regex is stored in the `matcher` value and shared among
   the strings.

GHC support
-----------

Only GHC versions >= 8.0 are supported, although older versions may work too.

[haddock]: http://hackage.haskell.org/packages/archive/regex-applicative/latest/doc/html/Text-Regex-Applicative.html
