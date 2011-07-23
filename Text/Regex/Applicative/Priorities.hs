{-# LANGUAGE DeriveFunctor #-}
module Text.Regex.Applicative.Priorities
    ( Priority
    , PrNum
    , withPriority
    , priorityToMaybe
    , isOK
    ) where
import Control.Applicative

-- | An applicative functor similar to Maybe, but it's '<|>' method honors
-- priority.
data Priority a = Priority { priority :: PrSeq, pValue :: a } | Fail
    deriving (Functor, Show)
type PrSeq = [PrNum]
type PrNum = Int

instance Applicative Priority where
    pure x = Priority [] x
    Priority p1 f <*> Priority p2 x = Priority ({-# SCC "concat_priorities" #-} p1 ++ p2) (f x)
    _ <*> _ = Fail

instance Alternative Priority where
    empty = Fail
    p@Priority {} <|> Fail = p
    Fail <|> p@Priority {} = p
    Fail <|> Fail = Fail
    p1@Priority {} <|> p2@Priority {} = {-# SCC "compare_priorities" #-}
        case compare (priority p1) (priority p2) of
            LT -> p2
            GT -> p1
            EQ -> error $
                "Two priorities are the same! Should not happen.\n" ++ show (priority p1)

-- | Adds priority to the end
withPriority :: PrNum -> Priority a -> Priority a
withPriority p (Priority ps x) = Priority ({-# SCC "snoc_priorities" #-}ps ++ [p]) x
withPriority _ Fail = Fail

-- | Discards priority information
priorityToMaybe :: Priority a -> Maybe a
priorityToMaybe p =
    case p of
        Priority { pValue = x } -> Just x
        Fail -> Nothing

isOK p =
    case p of
        Fail -> False
        Priority {} -> True
