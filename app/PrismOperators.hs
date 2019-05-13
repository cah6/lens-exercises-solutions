module PrismOperators where

import Data.Functor.Const
import Data.Monoid

infixl 8 ^?
(^?) :: s
     -> ((a -> Const (First a) b) -> s -> Const (First a) t)
     -> Maybe a
(^?) s prism = getFirst $ getConst $ (flip prism) s $ \a -> Const (First (Just a))

_Just :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
_Just f mA = traverse f mA