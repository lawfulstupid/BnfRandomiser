module BnfRandomiser.Data.Expression where

import BnfRandomiser.Data.Sequence

data Expression = Expr [Sequence] -- disjunctive list of options
