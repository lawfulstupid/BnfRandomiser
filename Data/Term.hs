module BnfRandomiser.Data.Term where

import BnfRandomiser.Data.Symbol

data Term = Sym Symbol | Lit String

