module TraversalOperators where

import Data.Functor.Const
import Data.Monoid

-- stuck TODO, not sure what undefined could be...
infixl 8 ^..
(^..) :: s
      -> ((a -> Const (Endo [a]) b) -> s -> Const (Endo [a]) t)
      -> [a]
(^..) s f = (appEndo $ getConst $ flip f s $ \a -> Const (Endo $ \xs -> a : xs)) undefined

sequenceAOf :: ((f b -> f b) -> s -> f t) -> s -> f t
sequenceAOf f s = flip f s id

traverseOf :: ((a -> f b) -> s -> f t) -> (a -> f b) -> s -> f t
traverseOf = id

traversed :: Applicative f => (a -> f b) -> [a] -> f [b]
traversed f xs = traverse f xs