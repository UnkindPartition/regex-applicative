{-# LANGUAGE ViewPatterns #-}
import RegExp
import Reference
import Control.Applicative
import Control.Monad
import Test.SmallCheck
import Data.Traversable
import Text.Printf

-- Small alphabets as SmallCheck's series
newtype A = A { a :: Char } deriving Show
instance Serial A where
    series = cons0 $ A 'a'
    coseries = error "No coseries, sorry"

newtype AB = AB { ab :: Char } deriving Show
instance Serial AB where
    series = cons0 (AB 'a') \/ cons0 (AB 'b')
    coseries = error "No coseries, sorry"

newtype ABC = ABC { abc :: Char } deriving Show
instance Serial ABC where
    series = cons0 (ABC 'a') \/ cons0 (ABC 'b') \/ cons0 (ABC 'c')
    coseries = error "No coseries, sorry"

re1 =
    let one = pure 1 <* sym 'a'
        two = pure 2 <* sym 'a' <* sym 'a'
    in (,) <$> (one <|> two) <*> (two <|> one)

re2 = sequenceA $
    [ pure 1 <* sym 'a' <* sym 'a' <|>
      pure 2 <* sym 'a'
    , pure 3 <* sym 'b'
    , pure 4 <* sym 'b' <|>
      pure 5 <* sym 'a' ]

re3 = sequenceA $
    [ pure 0 <|> pure 1
    , pure 1 <* sym 'a' <* sym 'a' <|>
      pure 2 <* sym 'a'
    , pure 3 <* sym 'b' <|> pure 6
    , fmap (+1) $
      pure 4 <* sym 'b' <|>
      pure 7 <|>
      pure 5 <* sym 'a' ]

re4 = sym 'a' *> many (sym 'b') <* sym 'a'

re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> many (sym 'a')

re6 = many (pure 3 <* sym 'a' <* sym 'a' <* sym 'a' <|> pure 1 <* sym 'a')

-- Regular expression from the weighted regexp paper.
re7 =
    let many_A_or_B = many (sym 'a' <|> sym 'b')
    in (,) <$>
        many ((,,,) <$> many_A_or_B <*> sym 'c' <*> many_A_or_B <*> sym 'c') <*>
        many_A_or_B


prop re f (map f -> s) = reference re s == s =~ re

tests =
   [ depthCheck 10 $ prop re1 a
   , depthCheck 10 $ prop re2 ab
   , depthCheck 10 $ prop re3 ab
   , depthCheck 10 $ prop re4 ab
   , depthCheck 10 $ prop re5 a
   , depthCheck 10 $ prop re6 a
   , depthCheck 7  $ prop re7 abc
   ]

main = do
    foldM runTest (1 :: Int) tests
    return ()
    where
    runTest n test = do
        printf "Running test case %d...\n" n
        test
        printf "\n"
        return $ n+1
