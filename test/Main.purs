module Main
  ( module Type.Reflection
  , module Data.Generic.Rep
  , None
  , Only, o
  , Pair, p
  , More, m
  , List, l
  , Tree, t
  ) where


import Type.Reflection

import Prelude ((+))
import Data.Generic.Rep (class Generic, from, to)
import Data.Maybe (Maybe)



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


data Pack
  = Pack (forall p. (forall a. Typeable a => a -> p) -> p)


pack :: forall a. Typeable a => a -> Pack
pack x = Pack \make -> make x


unpack :: forall a. Typeable a => Pack -> Maybe a
unpack (Pack unmake) = unmake \x -> cast x
