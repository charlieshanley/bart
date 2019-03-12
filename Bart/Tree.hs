{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Bart.Tree where

import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Prob)
import qualified Data.Array.Repa as R
import           Data.Array.Repa (Array, U, D,
                                  (:.)(..), Z(..), DIM1, DIM2, Any(..))
-- import           Control.Monad.Primitive (PrimMonad)
-- TODO bring back PrimMonad m => Prob m _ instead of Prob IO _

type X = Array U DIM2 Double

data Tree = Leaf   { obs       :: X }
          | Branch { covariate :: Int
                   , threshold :: Double
                   , lte       :: Tree
                   , gt        :: Tree
                   }


prior :: X -> Prob IO Tree
prior = prior' 0.95 0.05

prior' :: Double -> Double -> X -> Prob IO Tree
prior' α β = go 1
    where
        psplit :: Int -> Prob IO Bool
        psplit depth = P.bernoulli $ α * fromIntegral depth ** (negate β)

        prule :: X -> Prob IO (Int, Double)
        prule x = do
            let (Z :. _ :. nCol) = R.extent x
            v <- P.discreteUniform [0..(nCol - 1)]
            let x_v = R.slice x (Any :. v)
                mx  = R.foldAllS max 0 x_v
                mn  = R.foldAllS min 1 x_v
            c <- P.uniformR (mx, mn)
            return (v, c)

        go :: Int -> X -> Prob IO Tree
        go depth x = do
            split <- psplit depth
            if not split then return (Leaf x) else do
                (v, c) <- prule x
                case splitX v c x of
                  Nothing             -> return (Leaf x)
                  Just (below, above) ->
                      Branch v c <$> go (depth+1) below <*> go (depth+1) above

splitX :: Int -> Double -> X -> Maybe (X, X)
splitX = undefined
