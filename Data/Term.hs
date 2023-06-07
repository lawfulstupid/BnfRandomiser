{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Term where

import BnfRandomiser.Data.Symbol
import YAMP.Module

data Term = Sym Symbol | Lit String
   deriving (Show, Eq)

instance (MonadPlus m, Foldable m) => Parse m Char Term where
   parser = do peek anyChar >>= \nextChar -> if nextChar == '"' || nextChar == '\'' then literalParser else fmap Sym symbolParser

literalParser :: (MonadPlus m, Foldable m) => Parser m Char Term
literalParser = aux '"' <|> aux '\'' where
   aux q = do
      char q
      s <- greedy $ many $ charThat (/= q)
      char q
      pure $ Lit s
