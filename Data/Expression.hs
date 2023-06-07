{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Expression where

import BnfRandomiser.Data.Sequence
import YAMP.Module

data Expression = Expr [Sequence] -- disjunctive list of options
   deriving (Show)

instance (MonadPlus m, Foldable m) => Parse m Char Expression where
   parser = do
      seq <- parser
      exp <- many (optional hspace >> char '|' >> optional hspace >> parser)
      pure $ Expr (seq : exp)
