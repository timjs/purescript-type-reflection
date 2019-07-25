module Data.Anything
  ( Anything
  , pack
  , unpack
  , module Type.Reflection
  ) where

import Type.Reflection
import Data.Maybe (Maybe)

data Anything
  = Anything (forall p. (forall a. Reflect a => a -> p) -> p)

pack :: forall a. Reflect a => a -> Anything
pack x = Anything \make -> make x

unpack :: forall a. Reflect a => Anything -> Maybe a
unpack (Anything unmake) = unmake \x -> cast x
