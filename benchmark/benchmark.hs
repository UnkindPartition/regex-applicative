{-# LANGUAGE FlexibleInstances, TypeApplications, RankNTypes, CPP #-}
import Data.List
import Data.Traversable
import Data.Maybe
import Data.Void
import Control.Monad

import Criterion.Main

import Text.Regex.Applicative
import qualified Text.Parser.Combinators as PC
import qualified Text.Parser.Char as PC
import Control.DeepSeq
import qualified Text.Parsec as Parsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Parsers as MP
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString.Char8 as BS8

parser1 :: PC.CharParsing f => f [String]
parser1 = many $
  PC.try (PC.string "foo") <|>
  PC.try (PC.string "bar") <|>
  PC.string "baz"

str :: String
str = concat $ replicate 10 "foobarfoobarbaz"

benchmarkParser
  :: NFData a
  => (forall f . (PC.CharParsing f) => f a)
  -> [Benchmark]
benchmarkParser parser =
    [ bench "regex-applicative" $ nf (match parser) str
    , bench "parsec" $ nf (Parsec.parse parser "-") str
    , bench "megaparsec" $ nf (Megaparsec.parseMaybe @Void (MP.unParsecT parser)) str
    , bench "attoparsec" $ nf (Attoparsec.parseOnly parser) (BS8.pack str)
    ]

main = defaultMain $
  [ bgroup "parsing" (benchmarkParser parser1)
  , bgroup "recognizing" (benchmarkParser (void parser1))
  ]

-- instances
instance PC.Parsing (RE c) where
  try = id
  (<?>) = const
  unexpected _ = empty
  notFollowedBy = error "RE: notFollowedBy"
  eof = error "RE: eof"
instance PC.CharParsing (RE Char) where
  satisfy = psym
  char = sym
  anyChar = anySym
  string = string
instance NFData Parsec.ParseError where
  rnf = flip seq ()
