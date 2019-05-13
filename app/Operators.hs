module Operators where

import Control.Lens hiding ((.~), (%~), (^.))
import Data.Functor.Identity
import Data.Functor.Const
import Data.Text

import Main

infixr 4 .~
(.~) :: ((a -> Identity b) -> s -> Identity t)
     -> b
     -> s
     -> t
(.~) lens b s = runIdentity $ (flip lens) s $ \a -> Identity b

infixr 4 %~
(%~) :: ((a -> Identity b) -> s -> Identity t)
     -> (a -> b)
     -> s
     -> t
(%~) lens f s = runIdentity $ (flip lens) s $ \a -> Identity (f a)

infixl 8 ^.
(^.) :: s
     -> ((a -> Const a b) -> s -> Const a t)
     -> a
(^.) s lens = getConst $ (flip lens) s Const

name' :: Functor f 
  => (Text -> f Text) 
  -> User 
  -> f User
name' f u = fmap (\t -> u {_name = t} ) (f (_name u))
