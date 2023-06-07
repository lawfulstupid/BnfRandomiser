{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Rule where

import BnfRandomiser.Data.Expression
import BnfRandomiser.Data.Symbol
import YAMP.Module

data Rule = Rule Symbol Expression
   deriving (Show)

toTuple :: Rule -> (Symbol, Expression)
toTuple (Rule s e) = (s,e)

instance (MonadPlus m, Foldable m) => Parse m Char Rule where
   parser = do
      optional hspace
      sym <- symbolParser
      optional hspace
      char '='
      optional hspace
      exp <- parser
      optional hspace
      pure $ Rule sym exp
