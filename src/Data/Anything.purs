module Data.Anything
  ( Anything
  , pack
  , unpack
  , module Type.Reflection
  ) where

import Type.Reflection
-- import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

data Anything
  = Anything (forall p. (forall a. IsType a => a -> p) -> p)

-- derive instance genericAnything :: Generic (Anything a) _
pack :: forall a. IsType a => a -> Anything
pack x = Anything \make -> make x

unpack :: forall a. IsType a => Anything -> Maybe a
unpack (Anything unmake) = unmake \x -> cast x
