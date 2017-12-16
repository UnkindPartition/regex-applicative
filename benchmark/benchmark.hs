import Data.List
import Data.Traversable
import Data.Maybe

import Criterion.Main

import Text.Regex.Applicative

regex = sequenceA (replicate 500 $ sym 'a' <|> pure 'b') <* sequenceA (replicate 500 $ sym 'a')

main = defaultMain [bench "aaaaa" $ whnf (match regex) $ replicate 800 'a']
