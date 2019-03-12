{-# LANGUAGE LambdaCase #-}

module Bart.Tree where

import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Prob)
import           Control.Monad.Primitive (PrimMonad)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V

-- TODO make general / polymorphic maybe
-- IE `Ix a => covariate :: a` or `covariate :: a -> Double`

-- TODO try with Data.Array in another branch

type X = V.Vector (U.Vector Double)
-- each U.Vector must be the same length (not expressed at type level)

data Tree = Leaf   { obs       :: X }
          | Branch { covariate :: Int
                   , threshold :: Double
                   , lte       :: Tree
                   , gt        :: Tree
                   }


prior :: PrimMonad m => X -> Prob m Tree
prior = prior' 0.95 0.05

prior' :: PrimMonad m => Double -> Double -> X -> Prob m Tree
prior' α β x = go 1 x
    where
        go :: Int -> X -> Prob m Tree
        go depth x' = do
            split <- P.bernoulli $ α * fromIntegral depth ** (negate β)
            if split
               then do
                   t <- branch x'
                   case t of
                     Branch 
                     Leaf x'' = Leaf x''
               else return (Leaf x)

branch :: PrimMonad m => X -> Prob m Tree
branch x = do
    v <- P.discreteUniform [1 .. U.length (x V.! 1)]
    let x_v = fmap (U.! v) x
    c <- P.uniformR (V.minimum x_v, V.maximum x_v)
    
