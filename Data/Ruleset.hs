module BnfRandomiser.Data.Ruleset where

import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Symbol

import Data.Map.Lazy (Map)

type Ruleset = Map Symbol Expression

class Randomise a where
   randomise :: Ruleset -> a -> IO String
