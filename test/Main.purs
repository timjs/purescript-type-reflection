module Main
  ( module Data.Anything
  , module Data.Maybe
  , None
  , Only(..), o
  , Pair(..), p
  , More(..), m
  , List(..), l
  , Tree(..), t
  , d1, d2, da, df, run_a, run_f
  , fst'
  ) where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

import Data.Anything (class Reflect, class TypeEquals, Anything, Proxy(..), Reflection, Same, cast, decide, decideFrom, from, pack, refl, reflect, to, typeOf, unpack)



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


d1 :: Anything
d1 = pack 1

d2 :: Anything
d2 = pack 2

dp :: Anything
dp = pack (Pair 1 2)

da :: Anything
da = pack [3, 4]

df :: Anything
df = pack f2
  where
    f1 x = x * 2
    f2 x y = x + y * 2
    fp (Pair x y) = x + y * 2


pack_pair :: forall a b. Reflect a => Reflect b => Pair a b -> Anything
pack_pair = pack

fst' :: forall a b. Reflect a => Reflect b => Proxy b -> Anything -> Maybe a
fst' _ d =
  -- case unpack_pair d :: forall a' b'. Reflect a' => Reflect b' => Maybe (Pair a' b') of
  case unpack d :: Maybe (Pair a b) of
    Just (Pair x y) -> Just x
    Nothing -> Nothing
    -- Just (Pair x y) -> ?h
    --   -- | decide :: Maybe (Same a a') -> Just x
    -- Nothing -> Nothing

  -- where
  --   unpack_pair :: Anything -> Maybe (Pair a b)
  --   unpack_pair = unpack

-- fst' :: forall a b. Anything -> Maybe Anything
-- fst' dyn =
--   case un dyn of
--     Just p -> ?h1
--     Nothing -> Nothing
--   -- case unpack dyn :: forall a b. Reflect a => Reflect b => Maybe (Pair a b) of
--   -- -- case ?h of
--   --   Just (Pair a b) -> Just ?h --(pack a)
--   --   Nothing -> Nothing
--   where
--     un :: Reflect a => Reflect b => Anything -> Maybe (Pair a b)
--     un d = unpack d
--     re :: Reflect a => Pair a b -> Anything
--     re (Pair x y) = pack x


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
