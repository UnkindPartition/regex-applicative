{-# LANGUAGE OverloadedStrings #-}
import Text.Regex.Applicative
import Text.Regex.Applicative.Reference
import Control.Applicative
import Control.Monad
import Data.Traversable
import Data.Maybe
import Text.Printf

import Test.HUnit
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.Framework.Providers.HUnit

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

re8 = (,) <$> many (sym 'a' <|> sym 'b') <*> many (sym 'b' <|> sym 'c')

-- NB: we don't test these against the reference impl, 'cause it will loop!
re9 = many (sym 'a' <|> empty) <* sym 'b'
re10 = few (sym 'a' <|> empty) <* sym 'b'

prop re f s =
    let fs = map f s in
    reference re fs == (fs =~ re)

prop_withMatched =
    let re = withMatched $ many (string "a" <|> string "ba")
    in \str ->
        case map ab str =~ re of
            Nothing -> True
            Just (x, y) -> concat x == y

-- Because we have 2 slightly different algorithms for recognition and parsing,
-- we test that they agree
testRecognitionAgainstParsing re f s =
    let fs = map f s in
    isJust (fs =~ re) == isJust (fs =~ (re *> pure ()))

tests =
    [ testGroup "Engine tests"
       [ t "re1" 10 $ prop re1 a
       , t "re2" 10 $ prop re2 ab
       , t "re3" 10 $ prop re3 ab
       , t "re4" 10 $ prop re4 ab
       , t "re5" 10 $ prop re5 a
       , t "re6" 10 $ prop re6 a
       , t "re7"  7 $ prop re7 abc
       , t "re8"  7 $ prop re8 abc
       ]
    , testGroup "Recognition vs parsing"
       [ t "re1" 10 $ testRecognitionAgainstParsing re1 a
       , t "re2" 10 $ testRecognitionAgainstParsing re2 ab
       , t "re3" 10 $ testRecognitionAgainstParsing re3 ab
       , t "re4" 10 $ testRecognitionAgainstParsing re4 ab
       , t "re5" 10 $ testRecognitionAgainstParsing re5 a
       , t "re6" 10 $ testRecognitionAgainstParsing re6 a
       , t "re7"  7 $ testRecognitionAgainstParsing re7 abc
       , t "re8"  7 $ testRecognitionAgainstParsing re8 abc
       , t "re8" 10 $ testRecognitionAgainstParsing re9 ab
       , t "re8" 10 $ testRecognitionAgainstParsing re10 ab
       ]
    , testProperty "withMatched" prop_withMatched
    , testGroup "Tests for matching functions"
        [ testGroup "findFirstPrefix"
            [ u "t1"
                (findFirstPrefix ("a" <|> "ab") "abc")
                (Just ("a","bc"))
            , u "t2"
                (findFirstPrefix ("ab" <|> "a") "abc")
                (Just ("ab","c"))
            , u "t3"
                (findFirstPrefix "bc" "abc")
                Nothing
            ]
        , testGroup "findFirstInfix"
            [ u "t1"
                (findFirstInfix ("a" <|> "ab") "tabc")
                (Just ("t", "a","bc"))
            , u "t2"
                (findFirstInfix ("ab" <|> "a") "tabc")
                (Just ("t", "ab","c"))
            ]
        , testGroup "findLongestPrefix"
            [ u "t1"
                (findLongestPrefix ("a" <|> "ab") "abc")
                (Just ("ab","c"))
            , u "t2"
                (findLongestPrefix ("ab" <|> "a") "abc")
                (Just ("ab","c"))
            , u "t3"
                (findLongestPrefix "bc" "abc")
                Nothing
            ]
        , testGroup "findLongestInfix"
            [ u "t1"
                (findLongestInfix ("a" <|> "ab") "tabc")
                (Just ("t", "ab","c"))
            , u "t2"
                (findLongestInfix ("ab" <|> "a") "tabc")
                (Just ("t", "ab","c"))
            , u "t3"
                (findLongestInfix "bc" "tabc")
                (Just ("ta", "bc",""))
            ]
        , testGroup "findShortestPrefix"
            [ u "t1"
                (findShortestPrefix ("a" <|> "ab") "abc")
                (Just ("a","bc"))
            , u "t2"
                (findShortestPrefix ("ab" <|> "a") "abc")
                (Just ("a","bc"))
            , u "t3"
                (findShortestPrefix "bc" "abc")
                Nothing
            ]
        ]
    ]
    where
    t name n = withDepth n . testProperty name
    u name real ideal = testCase name (assertEqual "" real ideal)

main = defaultMain tests
