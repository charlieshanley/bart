module Bart.Tree where

-- TODO make general / polymorphic maybe

data Tree a =
    Branch { covariate :: a -> Double, threshold :: Double }
  | Leaf   { obs :: [a] }
