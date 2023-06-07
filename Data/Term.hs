{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Term where

import BnfRandomiser.Data.Symbol
import YAMP.Module

data Term = Sym Symbol | Lit String
   deriving (Show, Eq)

instance (MonadPlus m, Foldable m) => Parse m Char Term where
   parser = do
      nextChar <- peek anyChar
      if nextChar == '"' || nextChar == '\''
         then Lit <$> literalParser
         else Sym <$> symbolParser

literalParser :: (MonadPlus m, Foldable m) => Parser m Char String
literalParser = aux '"' <|> aux '\'' where
   aux q = do
      char q
      s <- greedy $ many $ charThat (/= q)
      char q
      pure s
