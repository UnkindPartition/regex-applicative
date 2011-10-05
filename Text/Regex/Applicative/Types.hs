{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Types where

newtype ThreadId = ThreadId Int
    deriving (Show, Eq, Ord, Num)

data Thread s a
    = Thread
        { threadId_ :: ThreadId
        , _threadCont :: s -> [Thread s a]
        }
    | Accept a

data Regexp s i a where
    Eps :: Regexp s i a
    Symbol :: i -> (s -> Bool) -> Regexp s i s
    Alt :: Regexp s i a -> Regexp s i a -> Regexp s i a
    App :: Regexp s i (a -> b) -> Regexp s i a -> Regexp s i b
    Fmap :: (a -> b) -> Regexp s i a -> Regexp s i b
    Rep :: (b -> a -> b) -- folding function (like in foldl)
        -> b             -- the value for zero matches, and also the initial value
                         -- for the folding function
        -> Regexp s i a
        -> Regexp s i b

