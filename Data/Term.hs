{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module BnfRandomiser.Data.Term where

import BnfRandomiser.Data.Symbol
import YAMP.Module

data Term = Sym Symbol | Lit String | Ref Int
   deriving (Show, Eq)

instance (MonadPlus m, Foldable m) => Parse m Char Term where
   parser = do
      nextChar <- peek anyChar
      case nextChar of
         '"' -> Lit <$> literalParser
         '\'' -> Lit <$> literalParser
         '$' -> Ref <$> backRefParser
         _ -> Sym <$> symbolParser

literalParser :: (MonadPlus m, Foldable m) => Parser m Char String
literalParser = aux '"' <|> aux '\'' where
   aux q = do
      char q
      s <- greedy $ many $ charThat (/= q)
      char q
      pure s

backRefParser :: (MonadPlus m, Foldable m) => Parser m Char Int
backRefParser = do
   match '$'
   read <$> greedy (some digit)

isBackRef :: Term -> Bool
isBackRef (Ref _) = True
isBackRef _ = False
