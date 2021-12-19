module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XLambdaCase"
  , "-XOverloadedStrings"
  , "Text/Regex/Applicative.hs"
  ]
