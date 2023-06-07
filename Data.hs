module BnfRandomiser.Data (
   Randomise(..),
   module BnfRandomiser.Data.File,
   module BnfRandomiser.Data.Expression,
   module BnfRandomiser.Data.Sequence,
   module BnfRandomiser.Data.Term,
   module BnfRandomiser.Data.Symbol
) where

import BnfRandomiser.Data.File
import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Sequence
import BnfRandomiser.Data.Term
import BnfRandomiser.Data.Symbol

import AbLib.System.Random (pick)
import Data.Map.Strict (Map, (!), (!?))

class Randomise a where
   randomise :: Ruleset -> a -> IO String

instance Randomise Expression where
   randomise mem (Expr opts) = do
      seq <- pick opts
      randomise mem seq

instance Randomise Sequence where
   randomise mem (Seq terms) = fmap concat $ sequence $ map (randomise mem) terms

instance Randomise Term where
   randomise _ (Lit str) = pure str
   randomise mem (Sym ident) = case mem !? ident of
      Nothing -> errorWithoutStackTrace ("Symbol '" ++ ident ++ "' not defined!")
      Just def -> randomise mem def
