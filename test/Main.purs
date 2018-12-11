module Main where


import Data.Generic.Rep



data None


data Only
  = Only


data More a b
  = Zero
  | One a
  | Two a b


derive instance genericVoid :: Generic None _
derive instance genericUnit :: Generic Only _
derive instance genericTest :: Generic (More a b) _
