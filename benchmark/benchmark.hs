import Text.Regex.Applicative
import Data.List
import Data.Traversable
import Data.Maybe

regex = sequenceA (replicate 500 $ sym 'a' <|> pure 'b') <* sequenceA (replicate 500 $ sym 'a')

main = do
    l <- getLine
    print $ isJust $ l =~ regex
