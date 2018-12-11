module Main where


import Data.Generic.Rep




data None


data Only
  = Only


data Pair a b
  = Pair a b


data More a b
  = Zero
  | One a
  | Two a b
  | Three (Array (Array (Pair a b)))


data List a
  = Nil
  | Cons a (List a)


derive instance genericVoid :: Generic None _
derive instance genericUnit :: Generic Only _
derive instance genericTest :: Generic (More a b) _
derive instance genericList :: Generic (List a) _
derive instance genericPair :: Generic (Pair a b) _
