module Bart.Proposal
    -- (
    -- )
        where

import qualified System.Random.MWC.Probability as P
import           System.Random.MWC.Probability (Prob)
import           Control.Monad.Primitive (PrimMonad)

data Proposal  = Birth  | Death | Swap | Change deriving (Eq, Show)
data Proposal' = Birth' | Death'

proposal :: PrimMonad m => Prob m Proposal
proposal =  fmap f P.uniform
    where f :: Double -> Proposal
          f x | x <= 0.25  = Birth
              | x <= 0.5   = Death
              | x <= 0.9   = Change
              | otherwise  = Swap

-- class Proposal p where
--     -- type family to specify model
--     proposalProb :: PrimMonad m => Prob m p
--     step         :: p -> model -> model

