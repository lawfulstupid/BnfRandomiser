module BnfRandomiser.Data.Symbol where

import YAMP.Module

type Symbol = String

symbolParser :: (MonadPlus m, Foldable m) => Parser m Char Symbol
symbolParser = do
   h <- letter
   t <- greedy $ many (alphanum <|> matchAny "_-")
   pure (h:t)
