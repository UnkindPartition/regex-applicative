{-# LANGUAGE Rank2Types #-}
module Text.Regex.Applicative.Interface where
import Control.Applicative hiding (empty)
import qualified Control.Applicative
import Data.Traversable
import Text.Regex.Applicative.Implementation

newtype RE s a = RE { unRE :: forall r . RegexpNode s r a }

instance Functor (RE s) where
    fmap f (RE a) = RE $ fmapNode f a

instance Applicative (RE s) where
    pure x = const x <$> RE epsNode
    (RE a1) <*> (RE a2) = RE $ RegexpNode
        { active = False
        , empty = empty a1 <*> empty a2
        , final_ = zero
        , reg = App a1 a2
        }

instance Alternative (RE s) where
    (RE a1) <|> (RE a2) = RE $ RegexpNode
        { active = False
        , empty = empty a1 `emptyChoice` empty a2
        , final_ = zero
        , reg = Alt a1 a2
        }
    empty = error "noMatch" <$> psym (const False)
    many a = reverse <$> reFoldl (flip (:)) [] a

psym :: (s -> Bool) -> RE s s
psym p = RE $ symbolNode p

sym :: Eq s => s -> RE s s
sym s = psym (s ==)

anySym :: RE s s
anySym = psym (const True)

string :: Eq a => [a] -> RE a [a]
string = sequenceA . map sym

reFoldl :: (b -> a -> b) -> b -> RE s a -> RE s b
reFoldl f b (RE a) = RE $ RegexpNode
    { active = False
    , empty = pure b
    , final_ = zero
    , reg = Rep f b a
    }

s =~ (RE r) = priorityToMaybe $ match r s
