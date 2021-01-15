module Data.Anything
  ( Anything
  , pack
  , unpack
  , module Type.Reflection
  ) where

import Type.Reflection
-- import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.Monoid ((<>))

data Anything
  = Anything (forall p. (forall a. IsType a => a -> p) -> p)

instance showAnything :: Show Anything where
  show (Anything unmake) = unmake \x -> "Anything :: " <> show (typeOf x)

-- derive instance genericAnything :: Generic (Anything a) _
pack :: forall a. IsType a => a -> Anything
pack x = Anything \make -> make x

unpack :: forall a. IsType a => Anything -> Maybe a
unpack (Anything unmake) = unmake \x -> cast x
