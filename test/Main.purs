module Main
  ( module Type.Reflection
  , None
  , Only, o
  , Pair, p
  , More, m
  , List, l
  , Tree, t
  , Dynamic, pack, unpack, d1, d2, da, df, run_a, run_f
  ) where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Type.Reflection (class TypeEquals, class Typeable, Proxy(..), Same, TypeRep, cast, cast', from, same, to, typeOf, typeRep)



-- Some types ------------------------------------------------------------------

-- None --


data None

derive instance genericNone :: Generic None _



-- Only --


data Only
  = Only

derive instance genericOnly :: Generic Only _


o :: Only
o = Only



-- Pair --


data Pair a b
  = Pair a b

derive instance genericPair :: Generic (Pair a b) _


p :: Pair Int Int
p = Pair 1 2



-- More --


data More a b
  = Zero
  | One a
  | Two a b

derive instance genericTest :: Generic (More a b) _


m :: More Int (Pair Int Int -> Int)
m = Two 1 (\(Pair x y) -> x + y)



-- List --


data List a
  = Nil
  | Cons a (List a)

derive instance genericList :: Generic (List a) _

l :: List Int
l = Cons 1 (Cons 0 Nil)



-- Tree --


data Tree a b
  = Leaf a
  | Node (Tree a b) b (Tree a b)

derive instance genericTree :: Generic (Tree a b) _


t :: Tree Int String
t = Node (Leaf 1) "a" (Leaf 2)



-- Existential typing ----------------------------------------------------------


data Dynamic
  = Dynamic (forall p. (forall a. Typeable a => a -> p) -> p)


pack :: forall a. Typeable a => a -> Dynamic
pack x = Dynamic \make -> make x


unpack :: forall a. Typeable a => Dynamic -> Maybe a
unpack (Dynamic unmake) = unmake \x -> cast x


d1 :: Dynamic
d1 = pack 1

d2 :: Dynamic
d2 = pack 2

da :: Dynamic
da = pack [3, 4]

df :: Dynamic
df = pack f2
  where
    f1 x = x * 2
    f2 x y = x + y * 2
    fp (Pair x y) = x + y * 2

run_a :: Maybe (Array Int)
run_a = ado
  x <- unpack d1
  y <- unpack d2
  a <- unpack da
  in [x, y] <> a

run_f :: Maybe Int
run_f = ado
  x <- unpack d1
  y <- unpack d2
  f <- unpack df :: Maybe (Int -> Int -> Int)
  in f x y
