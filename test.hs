import RegExp
import Reference
import Control.Applicative
import Test.SmallCheck
import Data.Traversable

re1 =
    let one = pure 1 <* sym ()
        two = pure 2 <* sym () <* sym ()
    in (,) <$> (one <|> two) <*> (two <|> one)

re2 = sequenceA $
    [ pure 1 <* sym True <* sym True <|>
      pure 2 <* sym True
    , pure 3 <* sym False
    , pure 4 <* sym False <|>
      pure 5 <* sym True ]

re3 = sequenceA $
    [ pure 0 <|> pure 1
    , pure 1 <* sym True <* sym True <|>
      pure 2 <* sym True
    , pure 3 <* sym False <|> pure 6
    , fmap (+1) $
      pure 4 <* sym False <|>
      pure 7 <|>
      pure 5 <* sym True ]

re4 = sym True *> many (sym False) <* sym True

re5 = (sym () <|> sym () *> sym ()) *> many (sym ())

re6 = many (pure 3 <* sym () <* sym () <* sym () <|> pure 1 <* sym ())

prop re s = reference re s == s =~ re

main = do
   smallCheck 10 $ prop re1
   smallCheck 10 $ prop re2
   smallCheck 10 $ prop re3
   smallCheck 10 $ prop re4
   smallCheck 10 $ prop re5
   smallCheck 10 $ prop re6
