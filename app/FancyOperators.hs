module FancyOperators where

import Control.Lens hiding ((<<>~), (<<<>~))



infixr 4 <<<>~
(<<<>~) :: Monoid a
        => ((a -> (a, a)) -> s -> (a, t))
        -> a
        -> s
        -> (a, t)
(<<<>~) = undefined
