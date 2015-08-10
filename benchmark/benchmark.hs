import Text.Regex.Applicative
import Data.List
import Data.Traversable
import Data.Maybe
import Criterion.Main

regex :: RE Char String
regex = sequenceA (replicate 500 $ sym 'a' <|> pure 'b') <* sequenceA (replicate 500 $ sym 'a')

matchRegex :: String -> Maybe String
matchRegex = match regex

a1000 :: String
a1000 = replicate 1000 'a'

main :: IO ()
main = defaultMain
  [ bgroup "basic" [ bench "all a" $ whnf matchRegex a1000 ]
  ]
